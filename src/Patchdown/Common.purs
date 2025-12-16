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
  , fieldWithDefault
  , fieldDimap
  , fieldWithDefaultSparse
  ) where

import Prelude

import Data.Argonaut (stringify)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec)
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Codec (Codec, Codec')
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Record as Record
import Type.Prelude (Proxy(..))

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

-- Parsing Helpers

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
