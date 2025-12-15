module Patchdown.Common
  ( ConvertError
  , ConvertResult
  , Converter
  , ConverterFields
  , logDebug
  , logDebug_
  , logError
  , logError_
  , logImpl
  , logInfo
  , logInfo_
  , mdBold
  , mdCodeBlock
  , mdH5
  , mdQuote
  , mdTicks
  , mkConvertError
  , mkConvertError_
  , mkConverter
  , print
  , printYaml
  , runConverter
  , yamlToJson
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec)
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

--- Converter

newtype MkConverter a = MkConverter (ConverterFields a)

type ConverterFields a =
  { name :: String
  , description :: String
  , codecJson :: JsonCodec a
  , printOpts :: a -> String
  , convert :: { opts :: a } -> Effect ConvertResult
  }

type ConvertResult =
  { content :: String
  , errors :: Array ConvertError
  }

type ConvertError =
  { message :: String
  , value :: Maybe Json
  }

mkConvertError_ :: String -> ConvertError
mkConvertError_ msg = { message: msg, value: Nothing }

mkConvertError :: forall a. EncodeJson a => String -> a -> ConvertError
mkConvertError msg val = { message: msg, value: Just $ encodeJson val }

newtype Converter = Converter (Exists MkConverter)

mkConverter :: forall a. ConverterFields a -> Converter
mkConverter fields = Converter $ mkExists $ MkConverter fields

runConverter :: forall r. Converter -> (forall a. ConverterFields a -> r) -> r
runConverter (Converter c) f = runExists ((\(MkConverter fields) -> fields) >>> f) c

-- Logging

logInfo :: forall m a. MonadEffect m => EncodeJson a => String -> String -> a -> m Unit
logInfo tag msg val = Console.log $ logImpl tag msg (Just $ encodeJson val)

logInfo_ :: forall m. MonadEffect m => String -> String -> m Unit
logInfo_ tag msg = Console.log $ logImpl tag msg Nothing

logDebug :: forall m a. MonadEffect m => EncodeJson a => String -> String -> a -> m Unit
logDebug tag msg val = pure unit

logDebug_ :: forall m. MonadEffect m => String -> String -> m Unit
logDebug_ tag msg = pure unit

logError :: forall m a. MonadEffect m => EncodeJson a => String -> String -> a -> m Unit
logError tag msg val = Console.log $ logImpl tag msg (Just $ encodeJson val)

logError_ :: forall m. MonadEffect m => String -> String -> m Unit
logError_ tag msg = Console.log $ logImpl tag msg Nothing

logImpl :: String -> String -> Maybe Json -> String
logImpl tag msg val =
  tag <> ": " <> msg <> case val of
    Just v -> " " <> stringify v
    Nothing -> ""

print :: forall a. EncodeJson a => String -> a -> String
print str val = str <> "\n\n" <> printYaml (encodeJson val)

-- Markdown Helpers

mdQuote :: String -> String
mdQuote str = str
  # Str.split (Pattern "\n")
  # map (\line -> "> " <> line)
  # Str.joinWith "\n"

mdBold :: String -> String
mdBold str = "**" <> str <> "**"

mdH5 :: String -> String
mdH5 str = "##### " <> str

mdTicks :: String -> String
mdTicks str = "`" <> Str.trim str <> "`"

mdCodeBlock :: String -> String -> String
mdCodeBlock lang str = "```" <> lang <> "\n" <> Str.trim str <> "\n```"

-- FFI

foreign import yamlToJson :: String -> Effect Json

foreign import printYaml :: Json -> String