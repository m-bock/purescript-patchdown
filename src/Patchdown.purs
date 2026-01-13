module Patchdown
  ( main
  , mainWithConfig
  , Config
  , defaultConfig
  ) where

import Prelude

import Control.Alt ((<|>))
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
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.String (Pattern(..))
import Data.String as Str
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags as RegFlags
import Data.Traversable (for, sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error, message)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, EffectFn5, mkEffectFn2, runEffectFn5)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Glob.Basic (expandGlobsCwd)
import Node.Process (lookupEnv)
import Node.Process as Process
import Patchdown.Common (ConvertError, Converter, ConverterContext, logDebug, mdCodeBlock, mdH5, mdQuote, mkConvertError, mkConvertError_, print, printYaml, runConverter, yamlToJson)
import Patchdown.Converters.Purs (defaultPursConfig, mkConverterPurs)
import Patchdown.Converters.Raw (converterRaw)

tag :: String
tag = "Patchdown"

mkStartTag :: String -> String
mkStartTag inner = "<!-- PD_START:" <> inner <> "-->"

endTag :: String
endTag = "<!-- PD_END -->"

regexPdSection :: Either String Regex
regexPdSection = regex
  ((mkStartTag "([a-zA-Z0-9_]*)(\\!?)(\\s[\\s\\S]*?)") <> "[\\s\\S]*?" <> endTag)
  RegFlags.global

type PdMatches =
  { converterName :: String
  , yamlStr :: String
  , enable :: Boolean
  }

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

getReplacement :: ConverterContext -> Map String Converter -> PdReplacementInputs -> ExceptT Err Effect PdReplacementOutputs
getReplacement context converterMap { converterName, yamlStr, enable } = do
  json <- yamlToJson yamlStr
    # mapErrEff \err -> InvalidYaml { err: message err }

  converter <- Map.lookup converterName converterMap
    # liftMaybe (InvalidConverter { json })

  runConverter converter \{ codecJson, printOpts, convert, name } -> do
    opts <- CA.decode codecJson json
      # lmap (\err -> InvalidOptions { json, err })
      # liftEither

    logDebug tag "parsed converter options" { name, opts: printOpts opts }

    let newYamlStr = yamlStr -- printYaml $ CA.encode codecJson opts

    { content, errors } <-
      if enable then do
        logDebug tag "run converter" { name }
        convert { opts, context } # mapErrEff (\err -> ConverterError { newYamlStr, err: message err })
      else do
        logDebug tag "skip converter" { name }
        pure { content: "", errors: [] }

    pure
      { newContent: content, newYamlStr, errors }

runFiles :: Config -> Aff { countErrors :: Int }
runFiles config = do
  files <- expandGlobsCwd config.globs
  converters <- liftEffect $ sequence config.mkConverters

  results <- for (Array.fromFoldable files) \filePath -> do

    let context = { filePath }

    liftEffect $ runFile { context, converters }

  pure { countErrors: sum $ map _.countErrors results }

runFile :: { context :: ConverterContext, converters :: Array Converter } -> Effect { countErrors :: Int }
runFile { context, converters } = do
  let converterMap = converters # map (\c -> runConverter c _.name /\ c) # Map.fromFoldable
  let converterNames = Map.keys converterMap

  logDebug tag "start" { context, converterNames }

  fileContent <- readTextFile UTF8 context.filePath

  { str: patchedFileContent, countErrors } <-
    replaceAllPdSections fileContent \opts -> do
      ret <- runExceptT $ getReplacement context converterMap opts
      pure $ mkPdReplacementOutputs opts { converterNames } ret

  writeTextFile UTF8 context.filePath patchedFileContent
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

type Config =
  { baseUrl :: Maybe String
  , globs :: Array String
  , mkConverters :: Array (Effect Converter)
  }

defaultConfig :: Config
defaultConfig =
  { baseUrl: Nothing
  , globs: [ "README.md" ]
  , mkConverters: [ pure converterRaw, mkConverterPurs defaultPursConfig ]
  }

type EnvVars =
  { "PATCHDOWN_GLOBS" :: Array String
  }

getEnvVars :: Effect EnvVars
getEnvVars = do
  globs <- lookupEnv "PATCHDOWN_GLOBS" <#> maybe [] (Str.split (Pattern ","))
  pure
    { "PATCHDOWN_GLOBS": globs
    }

mergeConfig :: { envVars :: EnvVars, userConfig :: Config } -> Config
mergeConfig { envVars: e, userConfig: c } =
  { baseUrl: c.baseUrl
  , globs: e."PATCHDOWN_GLOBS" <|> c.globs
  , mkConverters: c.mkConverters
  }

main :: Effect Unit
main = mainWithConfig defaultConfig

mainWithConfig :: Config -> Effect Unit
mainWithConfig userConfig = do
  envVars <- getEnvVars
  let config = mergeConfig { envVars, userConfig }
  launchAff_ do
    { countErrors } <- runFiles config
    unless (countErrors == 0) do
      log $ "errors found: " <> show countErrors
      liftEffect $ Process.exit' 1

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
      (converterName <> (if enable then "" else "!") <> newYamlStr)

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