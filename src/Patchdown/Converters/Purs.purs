module Patchdown.Converters.Purs
  ( mkConverterPurs
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
import Data.Codec (Codec, Codec')
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String as Str
import Data.String.Extra (snakeCase)
import Data.Symbol (class IsSymbol)
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
import Patchdown.Common (ConvertError, Converter, mdCodeBlock, mdTicks, mkConvertError, mkConverter)
import Prim.Row (class Cons, class Union)
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

mkConverterPurs :: Effect Converter
mkConverterPurs = do
  cache <- mkCache
  pure $ converterPurs cache

converterPurs :: Cache -> Converter
converterPurs cache = mkConverter
  { name: "purs"
  , description: "PureScript identifier converter"
  , codecJson: codecOpts
  , printOpts: show
  , convert: \opts -> do
      (content /\ errors) <- runWriterT $ convert cache opts
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

convert :: Cache -> { opts :: Opts } -> WriterT (Array ConvertError) Effect String
convert cache { opts: opts@{ pick } } = do
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
      Just filePath -> c <> mkFileLink filePath lineRange
      Nothing -> c

  pure $ addFileLink $ wrapNl $ wrapNl $ wrapOuter (Str.joinWith "\n" items')

summarizeLineRanges :: Array LineRange -> Maybe LineRange
summarizeLineRanges = foldl
  ( \acc { lineStart, lineEnd } -> case acc of
      Just { lineStart: accLineStart, lineEnd: accLineEnd } -> Just { lineStart: min accLineStart lineStart, lineEnd: max accLineEnd lineEnd }
      Nothing -> Just { lineStart, lineEnd }
  )
  Nothing

mkFileLink :: FilePath -> Maybe LineRange -> String
mkFileLink filePath lr =
  let
    rangePartLink = case lr of
      Just { lineStart, lineEnd } -> "#L" <> show (lineStart + 1) <> "-L" <> show (lineEnd + 1)
      Nothing -> ""

    rangePartLabel = case lr of
      Just { lineStart, lineEnd } -> " L" <> show (lineStart + 1) <> "-L" <> show (lineEnd + 1)
      Nothing -> ""
  in
    "\n\n<p align=\"right\"><sup>🗎 <a href=\"" <> filePath <> rangePartLink <> "\">" <> filePath <> rangePartLabel <> "</a></sup></p>"

--- Codecs

codecOpts :: JsonCodec Opts
codecOpts = CA.object "Opts" $
  CAR.record
    { filePath: CAR.optional CA.string
    , pick: CAR.optional (oneOrMany codecPickItemShorthand)
    , inline: CAR.optional CA.boolean
    , split: CAR.optional CA.boolean
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

---

fieldWithDefaultSparse
  :: forall @sym m x r r' t a b
   . IsSymbol sym
  => Monad m
  => Cons sym (Maybe x) t r
  => Cons sym x t r'
  => x
  -> (x -> Boolean)
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldWithDefaultSparse defDec shouldNotEncode = fieldDimap @sym
  (\val -> if shouldNotEncode val then Nothing else Just val)
  (fromMaybe defDec)

fieldWithDefault
  :: forall @sym m x r r' t a b
   . IsSymbol sym
  => Monad m
  => Cons sym (Maybe x) t r
  => Cons sym x t r'
  => x
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldWithDefault defDec = fieldDimap @sym Just (fromMaybe defDec)

fieldDimap
  :: forall @sym m x y r r' t a b
   . IsSymbol sym
  => Monad m
  => Cons sym x t r
  => Cons sym y t r'
  => (y -> x)
  -> (x -> y)
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldDimap f1 f2 codec = fieldCompose @sym (CA.codec' (f2 >>> pure) f1) codec

fieldCompose
  :: forall @sym m x y r r' t a b
   . Monad m
  => IsSymbol sym
  => Cons sym x t r
  => Cons sym y t r'
  => Codec' m x y
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldCompose codec1 codec2 = CA.codec dec enc
  where
  prx = Proxy :: Proxy sym

  dec :: a -> m (Record r')
  dec j = do
    rec :: Record r <- CA.decode codec2 j
    let val = Record.get prx rec :: x
    val' :: y <- CA.decode codec1 val
    let rec' = Record.set prx val' rec :: Record r'
    pure rec'

  enc :: Record r' -> b
  enc r =
    let
      rec = Record.modify prx (CA.encode codec1) r :: Record r
    in
      CA.encode codec2 rec

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

getNameIdent :: CST.Name CST.Ident -> String
getNameIdent (CST.Name { name: CST.Ident name }) = name

subsetFields :: forall r t r'. Union r t r' => Record r' -> Record r
subsetFields = unsafeCoerce

wrapNl :: String -> String
wrapNl str =
  "\n" <> str <> "\n"
