module Patchdown.Converters.Purs
  ( FileLinkParams
  , LineRange
  , PursConfig
  , Url
  , defaultPursConfig
  , mkConverterPurs
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Codec.Argonaut (JPropCodec, JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum (class GFlatCases)
import Data.Codec.Argonaut.Sum as CAS
import Data.Either (Either)
import Data.Foldable (foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String as Str
import Data.String.Extra (snakeCase)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception as Exc
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Obj
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NodeFS
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Patchdown.Common (ConvertError, Converter, ConverterContext, fieldWithDefault, fieldWithDefaultSparse, mdCodeBlock, mdTicks, mkConvertError, mkConverter)
import Prim.Row (class Union)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors as CSTErr
import PureScript.CST.Print as Print
import PureScript.CST.Range (class RangeOf, class TokensOf, rangeOf, tokensOf)
import PureScript.CST.Range.TokenList (TokenList)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST
import Record as Record
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Url = String

type FileLinkParams =
  { baseUrl :: Maybe Url
  , filePath :: FilePath
  , lineRange :: Maybe LineRange
  , url :: Url
  , label :: String
  }

mkFileLinkParams :: { baseUrl :: Maybe Url, filePath :: FilePath, lineRange :: Maybe LineRange } -> FileLinkParams
mkFileLinkParams { baseUrl, filePath, lineRange } =
  let
    lineRangeHuman = map (\lr -> lr + { lineStart: 1, lineEnd: 1 }) lineRange

    urlPrefix = maybe "" (\u -> u <> "/") baseUrl
    urlSuffix = maybe "" (\{ lineStart, lineEnd } -> "#L" <> show lineStart <> "-L" <> show lineEnd) lineRangeHuman

    labelSuffix = maybe "" (\{ lineStart, lineEnd } -> " (lines " <> show lineStart <> "-" <> show lineEnd <> ")") lineRangeHuman

    url = urlPrefix <> filePath <> urlSuffix
    label = filePath <> labelSuffix
  in

    { baseUrl
    , filePath
    , lineRange
    , url
    , label
    }

type PursConfig =
  { renderFileLink :: FileLinkParams -> String
  , fileLinkBaseUrl :: Maybe Url
  }

defaultPursConfig :: PursConfig
defaultPursConfig =
  { renderFileLink: ghStyleHtmlFileLink
  , fileLinkBaseUrl: Nothing
  }

ghStyleHtmlFileLink :: FileLinkParams -> String
ghStyleHtmlFileLink { url, label } = "<a href=\"" <> url <> "\">" <> label <> "</a>"

--- Cache

type Cache = { getCst :: String -> Effect (Array Source) }

getModuleCst :: String -> Effect (CST.Module Void)
getModuleCst content = case parseModule content of
  ParseSucceeded cst ->
    pure cst
  ParseSucceededWithErrors _ _ ->
    throwError $ Exc.error "success with errors"
  ParseFailed { error } ->
    throwError $ Exc.error $ CSTErr.printParseError error

mkCache :: Effect Cache
mkCache = do
  refCache <- Ref.new (Map.empty :: Map String (Array Source))
  pure
    { getCst: \filePath -> do
        cache <- Ref.read refCache
        case Map.lookup filePath cache of
          Just sources -> pure sources
          Nothing -> do
            content <- NodeFS.readTextFile UTF8 filePath
            cst <- getModuleCst content
            let sources = getSources cst
            Ref.modify_ (Map.insert content sources) refCache
            pure sources
    }

--- Core

type Opts =
  { filePath :: Maybe String
  , inline :: Boolean
  , split :: Boolean
  , pick :: Array PickItem
  , maxLines :: Maybe Int
  }

type PickItem =
  { filePath :: Maybe String
  , prefix :: Maybe String
  , pick :: Pick
  }

data Pick
  = PickImport
      { moduleName :: Maybe String }
  | PickData
      { name :: String }
  | PickNewtype
      { name :: String }
  | PickType
      { name :: String }
  | PickSignature
      { name :: String }
  | PickForeignValue
      { name :: String, stripImport :: Boolean }
  | PickValue
      { name :: String }
  | PickInstance
      { className :: String, typeName :: String }
  | PickExtraTypeRecord
      { name :: String }

  -- | PickExtraDataOrNewtype
  --     { name :: String }
  | PickExtraSignatureOrForeign
      { name :: String }
  | PickExtraValueAndSignature
      { name :: String }
  | PickExtraAny
      { name :: String
      }

-- PickModuleHeader
-- PickClass { name :: String, filePath :: Maybe String }
-- PickInstancechain
-- PickInstance
-- PickDerived
-- PickKindSignature
-- PickFixity
-- PickRole

derive instance Eq Pick
derive instance Generic Pick _

instance Show Pick where
  show = genericShow

instance EncodeJson Pick where
  encodeJson = CA.encode codecPick

data Source
  = SrcImport (CST.ImportDecl Void)
  | SrcDecl (CST.Declaration Void)

getSources :: CST.Module Void -> Array Source
getSources (CST.Module { header, body }) =
  let
    CST.ModuleHeader { imports } = header
    CST.ModuleBody { decls } = body
  in
    map SrcImport imports <> map SrcDecl decls

type NameInfo =
  { imports :: Array String
  , dataTypes :: Array String
  , newtypes :: Array String
  , types :: Array String
  , signatures :: Array String
  , foreignValues :: Array String
  , values :: Array String
  }

getNames :: Array Source -> NameInfo
getNames =
  foldl
    ( \accum -> case _ of
        SrcDecl (CST.DeclData r _) -> Record.modify
          (Proxy :: _ "dataTypes")
          (_ <> [ getNameProper r.name ])
          accum

        SrcDecl (CST.DeclNewtype r _ _ _) -> Record.modify
          (Proxy :: _ "newtypes")
          (_ <> [ getNameProper r.name ])
          accum

        SrcDecl (CST.DeclType r _ _) -> Record.modify
          (Proxy :: _ "types")
          (_ <> [ getNameProper r.name ])
          accum

        SrcDecl (CST.DeclSignature (CST.Labeled r)) -> Record.modify
          (Proxy :: _ "signatures")
          (_ <> [ getNameIdent r.label ])
          accum

        SrcDecl (CST.DeclForeign _ _ (CST.ForeignValue (CST.Labeled r))) -> Record.modify
          (Proxy :: _ "foreignValues")
          (_ <> [ getNameIdent r.label ])
          accum

        SrcDecl (CST.DeclForeign _ _ (CST.ForeignData _ _)) -> accum

        SrcDecl (CST.DeclForeign _ _ (CST.ForeignKind _ _)) -> accum

        SrcDecl (CST.DeclValue r) -> Record.modify
          (Proxy :: _ "values")
          (_ <> [ getNameIdent r.name ])
          accum

        SrcDecl _ -> accum

        SrcImport _ -> accum
    )
    mempty

matchOnePick :: Pick -> Source -> Array { content :: String, lineRange :: LineRange }
matchOnePick pick decl = case pick of
  PickImport _ -> []
  PickData { name } -> case decl of
    SrcDecl all@(CST.DeclData r _)
      | name == getNameProper r.name -> [ printTokens all # addContent "\n" ]
    _ -> []

  PickNewtype { name } -> case decl of
    SrcDecl all@(CST.DeclNewtype r _ _ _)
      | name == getNameProper r.name -> [ printTokens all # addContent "\n" ]
    _ -> []

  PickType { name } -> case decl of
    SrcDecl all@(CST.DeclType r _ _)
      | name == getNameProper r.name -> [ printTokens all # addContent "\n" ]
    _ -> []

  PickSignature { name } -> case decl of
    SrcDecl all@(CST.DeclSignature (CST.Labeled r))
      | name == getNameIdent r.label -> [ printTokens all ]
    _ -> []

  PickForeignValue { name, stripImport } -> case decl of
    SrcDecl all@(CST.DeclForeign v1 v2 v3@(CST.ForeignValue (CST.Labeled r)))
      | name == getNameIdent r.label ->
          [ ( if stripImport then printTokens v3
              else printTokens all
            )
              # addContent "\n"
          ]
    _ -> []

  PickValue { name } -> case decl of
    SrcDecl all@(CST.DeclValue r)
      | name == getNameIdent r.name -> [ printTokens all # addContent "\n" ]
    _ -> []

  PickInstance { className, typeName } -> case decl of
    SrcDecl
      all@
        ( CST.DeclInstanceChain
            ( CST.Separated
                { head: CST.Instance
                    { head:
                        { className: c
                        , types: [ CST.TypeConstructor t ]
                        }
                    }
                }
            )
        )
      | className == getQualNameProper c && typeName == getQualNameProper t -> [ printTokens all # addContent "\n" ]
    _ -> []

  PickExtraTypeRecord { name } -> [] -- TODO3

  PickExtraValueAndSignature { name } ->
    matchManyPicks
      [ PickValue { name }
      , PickSignature { name }
      ]
      decl

  PickExtraSignatureOrForeign { name } ->
    matchManyPicks
      [ PickSignature { name }
      , PickForeignValue { name, stripImport: true }
      ]
      decl

  PickExtraAny { name } ->
    matchManyPicks
      [ PickImport { moduleName: Just name }
      , PickData { name }
      , PickNewtype { name }
      , PickType { name }
      , PickSignature { name }
      , PickForeignValue { name, stripImport: false }
      , PickValue { name }
      ]
      decl

matchManyPicks :: Array Pick -> Source -> Array { content :: String, lineRange :: LineRange }
matchManyPicks picks decl = foldMap (\p -> matchOnePick p decl) picks

type EnvVars =
  { "PATCHDOWN_PURS_FILE_LINK_BASE_URL" :: Maybe Url
  }

getEnvVars :: Effect EnvVars
getEnvVars = do
  fileLinkBaseUrl <- lookupEnv "PATCHDOWN_PURS_FILE_LINK_BASE_URL"
  pure
    { "PATCHDOWN_PURS_FILE_LINK_BASE_URL": fileLinkBaseUrl
    }

mergeConfig :: { envVars :: EnvVars, userConfig :: PursConfig } -> PursConfig
mergeConfig { envVars: e, userConfig: c } =
  { fileLinkBaseUrl: e."PATCHDOWN_PURS_FILE_LINK_BASE_URL" <|> c.fileLinkBaseUrl
  , renderFileLink: c.renderFileLink
  }

mkConverterPurs :: PursConfig -> Effect Converter
mkConverterPurs userConfig = do
  envVars <- getEnvVars
  let config = mergeConfig { envVars, userConfig }
  cache <- mkCache
  pure $ converterPurs cache config

converterPurs :: Cache -> PursConfig -> Converter
converterPurs cache config = mkConverter
  { name: "purs"
  , description: "PureScript identifier converter"
  , codecJson: codecOpts
  , printOpts: show
  , convert: \opts -> do
      (content /\ errors) <- runWriterT $ convert cache config opts
      pure { content, errors }
  }

getWrapFn :: Opts -> { wrapInner :: String -> String, wrapOuter :: String -> String }
getWrapFn { split, inline } =
  let
    wrapFn = if inline then mdTicks else mdCodeBlock "purescript"
  in
    { wrapInner: if split then wrapFn else identity
    , wrapOuter: if split then identity else wrapFn
    }

convert :: Cache -> PursConfig -> { opts :: Opts, context :: ConverterContext } -> WriterT (Array ConvertError) Effect String
convert cache config { opts: opts@{ pick } } = do
  let { wrapInner, wrapOuter } = getWrapFn opts

  items :: (Array { lineRange :: Maybe LineRange, content :: String }) <- for pick
    ( \{ pick, filePath, prefix } -> do
        sources <- liftEffect $ cache.getCst (fromMaybe "src/Main.purs" (filePath <|> opts.filePath))
        let results = sources >>= matchOnePick pick

        when (results == []) do
          tell
            [ mkConvertError "no values found" { pick, names: getNames sources } ]

        let
          addPrefix val = case prefix of
            Just p -> (p <> val)
            Nothing -> val

          items = map (addPrefix <<< wrapInner) (map _.content results)

          lineRange = summarizeLineRanges (map _.lineRange results)

        pure { lineRange, content: Str.joinWith "\n" items }
    )

  let
    lineRange = summarizeLineRanges (mapMaybe _.lineRange items)
    items' = map _.content items
    addFileLink c = case opts.filePath of
      Just filePath ->
        let
          fileLinkParams = mkFileLinkParams { baseUrl: config.fileLinkBaseUrl, filePath, lineRange }
        in
          c <> config.renderFileLink fileLinkParams
      Nothing -> c

    cutLines content = case opts.maxLines of
      Just maxLines ->
        let
          lines = Str.split (Pattern "\n") content
          omittedLinesCount = Array.length lines - maxLines
        in
          lines
            # Array.take maxLines
            # Str.joinWith "\n"
            # \c -> c <> "\n\n" <> "-- And so on ... (" <> show omittedLinesCount <> " lines omitted)"
      Nothing -> content

  pure $ addFileLink $ wrapNl $ wrapNl $ wrapOuter $ cutLines $ Str.joinWith "\n" items'

summarizeLineRanges :: Array LineRange -> Maybe LineRange
summarizeLineRanges = foldl
  ( \acc { lineStart, lineEnd } -> case acc of
      Just { lineStart: accLineStart, lineEnd: accLineEnd } -> Just { lineStart: min accLineStart lineStart, lineEnd: max accLineEnd lineEnd }
      Nothing -> Just { lineStart, lineEnd }
  )
  Nothing

--- Codecs

codecOpts :: JsonCodec Opts
codecOpts = CA.object "Opts" $
  CAR.record
    { filePath: CAR.optional CA.string
    , pick: CAR.optional (oneOrMany codecPickItemShorthand)
    , inline: CAR.optional CA.boolean
    , split: CAR.optional CA.boolean
    , maxLines: CAR.optional CA.int
    }
    # fieldWithDefaultSparse @"split" false not
    # fieldWithDefaultSparse @"inline" false not
    # fieldWithDefault @"pick" []

mkPickItem :: Pick -> PickItem
mkPickItem pick =
  { filePath: Nothing
  , prefix: Nothing
  , pick
  }

codecPickItemShorthand :: JsonCodec PickItem
codecPickItemShorthand = CA.codec' dec enc
  where
  dec j =
    ( do
        str <- CA.decode CA.string j
        pure $ mkPickItem $ PickExtraAny { name: str }
    ) <|> do
      CA.decode codecPickItem j

  enc val =
    case val.pick of
      PickExtraAny { name } | mkPickItem val.pick == val -> CA.encode CA.string name
      _ -> CA.encode codecPickItem val

codecPickItem :: JsonCodec PickItem
codecPickItem = CA.object "PickItem" $ CA.codec dec enc
  where
  dec obj = do
    pick <- CA.decode jpropCodecPick obj
    fields <- CA.decode codecFields obj

    pure $ Record.insert (Proxy :: _ "pick") pick fields

  enc val =
    let
      pick = CA.encode jpropCodecPick val.pick
      fields = CA.encode codecFields (subsetFields val)
    in
      pick <> fields

  codecFields =
    CAR.record
      { filePath: CAR.optional CA.string
      , prefix: CAR.optional CA.string
      }

sumFlatWith'
  :: forall @tag r rep a
   . GFlatCases tag r rep
  => Generic a rep
  => { mapTag :: String -> String
     , tag :: Proxy tag
     }
  -> String
  -> Record r
  -> JPropCodec a
sumFlatWith' opt name r = CA.codec dec enc
  where
  dec :: Object Json -> Either JsonDecodeError a
  dec obj = CA.decode codec (encodeJson obj)

  enc :: a -> List (String /\ Json)
  enc val =
    let
      j :: Json
      j = CA.encode codec val

      obj :: Object Json
      obj = unsafeCoerce j
    in
      Obj.toUnfoldable obj

  codec = CAS.sumFlatWith opt name r

codecPick :: JsonCodec Pick
codecPick = CA.object "Pick" jpropCodecPick

jpropCodecPick :: JPropCodec Pick
jpropCodecPick = sumFlatWith'
  CAS.defaultFlatEncoding
    { mapTag = replace (Pattern "Pick") (Replacement "")
        >>> replace (Pattern "Extra") (Replacement "")
        >>> snakeCase
    }
  "Pick"
  { "PickImport": CAR.record
      { moduleName: CAR.optional CA.string
      }
  , "PickData": CAR.record
      { name: CA.string
      }
  , "PickNewtype": CAR.record
      { name: CA.string
      }
  , "PickType": CAR.record
      { name: CA.string
      }
  , "PickSignature": CAR.record
      { name: CA.string
      }
  , "PickForeignValue":
      CAR.record
        { name: CA.string
        , stripImport: CAR.optional CA.boolean
        }
        # fieldWithDefault @"stripImport" false
  , "PickValue": CAR.record
      { name: CA.string
      }
  , "PickInstance": CAR.record
      { className: CA.string
      , typeName: CA.string
      }
  , "PickExtraTypeRecord": CAR.record
      { name: CA.string
      }
  , "PickExtraSignatureOrForeign": CAR.record
      { name: CA.string
      }
  , "PickExtraValueAndSignature": CAR.record
      { name: CA.string
      }
  , "PickExtraAny": CAR.record
      { name: CA.string
      }
  }

--- Utils

printTokens :: forall a. TokensOf a => RangeOf a => a -> { content :: String, lineRange :: LineRange }
printTokens cst =
  { content: printTokenList (tokensOf cst)
  , lineRange: sourceRangeToLineRange (rangeOf cst)
  }

addContent :: String -> { content :: String, lineRange :: LineRange } -> { content :: String, lineRange :: LineRange }
addContent str { content, lineRange } = { content: content <> str, lineRange }

sourceRangeToLineRange :: CST.SourceRange -> LineRange
sourceRangeToLineRange { start, end } = { lineStart: start.line, lineEnd: end.line }

type LineRange = { lineStart :: Int, lineEnd :: Int }

printTokenList :: TokenList -> String
printTokenList tokenList =
  let
    sourceTokens =
      TokenList.toArray tokenList
        # Array.modifyAtIndices [ 0 ] (\r -> r { leadingComments = [] })

  in
    foldMap Print.printSourceToken sourceTokens
      # Str.trim

altDec :: forall a. (Json -> Either JsonDecodeError a) -> JsonCodec a -> JsonCodec a
altDec dec c = CA.codec' dec' enc
  where
  dec' :: Json -> Either JsonDecodeError a
  dec' j = dec j <|> CA.decode c j

  enc :: a -> Json
  enc = CA.encode c

oneOrMany :: forall a. JsonCodec a -> JsonCodec (Array a)
oneOrMany c = altDec (map Array.singleton <<< CA.decode c) (CA.array c)

getNameProper :: CST.Name CST.Proper -> String
getNameProper (CST.Name { name: CST.Proper name }) = name

getQualNameProper :: CST.QualifiedName CST.Proper -> String
getQualNameProper (CST.QualifiedName { name: CST.Proper name }) = name

getNameIdent :: CST.Name CST.Ident -> String
getNameIdent (CST.Name { name: CST.Ident name }) = name

subsetFields :: forall r t r'. Union r t r' => Record r' -> Record r
subsetFields = unsafeCoerce

wrapNl :: String -> String
wrapNl str =
  "\n" <> str <> "\n"
