module Patchdown.Converters.Raw where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Patchdown.Common (ConvertResult, Converter, fieldWithDefaultSparse, mkConverter)
import Data.Maybe (Maybe, fromMaybe)

type Opts =
  { filePath :: String
  , wrapNl :: Boolean
  , prefix :: Maybe String
  , suffix :: Maybe String
  }

converterRaw :: Converter
converterRaw = mkConverter
  { name: "raw"
  , description: "Raw converter"
  , codecJson: codecOpts
  , printOpts: show
  , convert: \{ opts } -> convert opts
  }

convert :: Opts -> Effect ConvertResult
convert { filePath, wrapNl, prefix, suffix } = do
  content <- readTextFile UTF8 filePath
  let content' = if wrapNl then "\n\n" <> content <> "\n" else content
  pure { content: prefix' <> content' <> suffix', errors: [] }
  where
  prefix' = fromMaybe "" prefix
  suffix' = fromMaybe "" suffix

codecOpts :: JsonCodec Opts
codecOpts =
  CAR.object "Opts"
    { filePath: CA.string
    , wrapNl: CAR.optional CA.boolean
    , prefix: CAR.optional CA.string
    , suffix: CAR.optional CA.string
    }
    # fieldWithDefaultSparse @"wrapNl" false not