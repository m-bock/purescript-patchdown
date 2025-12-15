module Patchdown
  ( main
  , run
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, liftEither, liftMaybe, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Argonaut.Core (Json)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Foldable (fold, sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Set (Set)
import Data.String as Str
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags as RegFlags
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error, message)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, EffectFn5, mkEffectFn2, runEffectFn5)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (lookupEnv)
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import Patchdown.Common (ConvertError, Converter, logDebug, logInfo, mdCodeBlock, mdH5, mdQuote, mkConvertError, mkConvertError_, print, printYaml, runConverter, yamlToJson)
import Patchdown.Converters.Purs (mkConverterPurs)
import Patchdown.Converters.Raw (converterRaw)

tag :: String
tag = "Patchdown"

type Opts =
  { filePath :: String
  , converters :: Array Converter
  }

mkStartTag :: String -> String
mkStartTag inner = "<!-- PD_START:" <> inner <> "-->"

endTag :: String
endTag = "<!-- PD_END -->"

regexPdSection :: Either String Regex
regexPdSection = regex
  (mkStartTag "([a-zA-Z0-9_]*)(\\!?)\\s([\\s\\S]*?)" <> "[\\s\\S]*?" <> endTag)
  RegFlags.global

type PdMatches = { converterName :: String, yamlStr :: String, enable :: Boolean }

data Err
  = InvalidYaml { err :: String }
  | InvalidOptions { json :: Json, err :: JsonDecodeError }
  | InvalidConverter { json :: Json }
  | ConverterError { newYamlStr :: String, err :: String }

type PdReplacementInputs =
  { converterName :: String
  , yamlStr :: String
  , enable :: Boolean
  }

type PdReplacementOutputs =
  { newYamlStr :: String
  , newContent :: String
  , errors :: Array ConvertError
  }

replaceAllPdSections
  :: String
  -> (PdReplacementInputs -> Effect PdReplacementOutputs)
  -> Effect { str :: String, countErrors :: Int }
replaceAllPdSections content replaceFn = do
  reg <- regexPdSection
    # lmap (\_ -> error "invalid regex")
    # liftEither

  (str /\ vals) <- replaceEffect reg
    ( \_ matches -> do
        pdMatches <- parseMatches matches
          # liftMaybe (error $ print "invalid matches" { matches })

        relaceOutputs@{ errors } <- replaceFn pdMatches

        let
          str = mdPdSection pdMatches relaceOutputs

        pure (str /\ Array.length errors)
    )
    content

  pure { str, countErrors: sum vals }

parseMatches :: Array (Maybe String) -> Maybe PdMatches
parseMatches matches =
  case matches of
    [ Just converterName, Just exclam, Just yamlStr ] ->
      Just { converterName, enable: exclam /= "!", yamlStr }
    _ -> Nothing

getReplacement :: Map String Converter -> PdReplacementInputs -> ExceptT Err Effect PdReplacementOutputs
getReplacement converterMap { converterName, yamlStr, enable } = do
  json <- yamlToJson yamlStr
    # mapErrEff \err -> InvalidYaml { err: message err }

  converter <- Map.lookup converterName converterMap
    # liftMaybe (InvalidConverter { json })

  runConverter converter \{ codecJson, printOpts, convert, name } -> do
    opts <- CA.decode codecJson json
      # lmap (\err -> InvalidOptions { json, err })
      # liftEither

    logDebug tag "parsed converter options" { name, opts: printOpts opts }

    let newYamlStr = printYaml $ CA.encode codecJson opts

    { content, errors } <-
      if enable then do
        logDebug tag "run converter" { name }
        convert { opts } # mapErrEff (\err -> ConverterError { newYamlStr, err: message err })
      else do
        logDebug tag "skip converter" { name }
        pure { content: "", errors: [] }

    pure
      { newContent: content, newYamlStr, errors }

run :: Opts -> Effect { countErrors :: Int }
run { filePath, converters } = do
  let converterMap = converters # map (\c -> runConverter c _.name /\ c) # Map.fromFoldable
  let converterNames = Map.keys converterMap

  logDebug tag "start" { filePath, converterNames }

  fileContent <- readTextFile UTF8 filePath

  { str: patchedFileContent, countErrors } <-
    replaceAllPdSections fileContent \opts -> do
      ret <- runExceptT $ getReplacement converterMap opts
      pure $ mkPdReplacementOutputs opts { converterNames } ret

  writeTextFile UTF8 filePath patchedFileContent
  pure { countErrors }

mkPdReplacementOutputs
  :: PdReplacementInputs
  -> { converterNames :: Set String }
  -> Either Err PdReplacementOutputs
  -> PdReplacementOutputs
mkPdReplacementOutputs { yamlStr, converterName } { converterNames } = case _ of
  Left (InvalidYaml { err }) ->
    { newYamlStr: yamlStr
    , newContent: ""
    , errors: [ mkConvertError_ err ]
    }
  Left (InvalidOptions { json, err }) ->
    { newYamlStr: printYaml json
    , newContent: ""
    , errors: [ mkConvertError (printJsonDecodeError err) { json } ]
    }
  Left (InvalidConverter { json }) ->
    { newYamlStr: printYaml json
    , newContent: ""
    , errors: [ mkConvertError "Converter not found" { converterName, converterNames } ]
    }
  Left (ConverterError { newYamlStr, err }) ->
    { newYamlStr
    , newContent: ""
    , errors: [ mkConvertError_ err ]
    }
  Right val -> val

main :: Effect Unit
main = do
  filePath <- lookupEnv "PATCHDOWN_FILE_PATH"
    # map (fromMaybe' \_ -> unsafeCrashWith "PATCHDOWN_FILE_PATH not set")

  pursConverter <- mkConverterPurs

  let converters = [ pursConverter ] <> [ converterRaw ]

  let
    opts = { filePath, converters }

  { countErrors } <- run opts

  unless (countErrors == 0) do
    log $ "errors found: " <> show countErrors
    Process.exit' 1

-- Markdown

mdErrorBox :: String -> String -> Maybe Json -> String
mdErrorBox sectionName message val = wrapNl $ mdQuote $ Str.joinWith "\n"
  [ "<br>"
  , "🛑 Error at section `" <> sectionName <> "`"
  , ""
  , mdH5 message
  , case val of
      Just v -> mdCodeBlock "yaml" (printYaml v)
      Nothing -> ""
  , "<br>"
  ]

mdPdSection :: PdMatches -> PdReplacementOutputs -> String
mdPdSection { converterName, enable } { newYamlStr, errors, newContent } =
  let
    startTag = mkStartTag
      (converterName <> (if enable then "" else "!") <> "\n" <> newYamlStr)

    errorBoxes = foldMap
      (\err -> mdErrorBox converterName (err.message) err.value)
      errors

  in
    fold
      [ startTag
      , errorBoxes
      , newContent
      , endTag
      ]

--- Utils

mapErrEff :: forall e m a. MonadEffect m => MonadError e m => (Error -> e) -> Effect a -> m a
mapErrEff mpErr eff = do
  ret :: Either Error a <- liftEffect (try eff)
  case ret of
    Left err -> throwError (mpErr err)
    Right val -> pure val

replaceEffect :: forall a. Regex -> (String -> Array (Maybe String) -> Effect (String /\ a)) -> String -> Effect (String /\ Array a)
replaceEffect reg replFn oldStr = do
  refVals <- Ref.new []
  newStr <- runEffectFn5 replaceEffectImpl Just Nothing reg
    ( mkEffectFn2 \match matches -> do
        (str /\ val) <- replFn match matches
        Ref.modify_ (_ <> [ val ]) refVals
        pure str
    )
    oldStr

  vals <- Ref.read refVals
  pure (newStr /\ vals)

wrapNl :: String -> String
wrapNl str = "\n" <> str <> "\n"

--- FFI

foreign import replaceEffectImpl
  :: EffectFn5
       (forall r. r -> Maybe r)
       (forall r. Maybe r)
       Regex
       (EffectFn2 String (Array (Maybe String)) String)
       String
       String