{-# OPTIONS_GHC -Wno-orphans #-}

module Tomcab.Utils.TOML (
  getFieldOr,
  applyDecoder,
  getAllExcept,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import TOML
import UnliftIO.Exception (Exception (..))

-- https://github.com/brandonchinn178/toml-reader/issues/13
instance Exception TOMLError where
  displayException = Text.unpack . renderTOMLError

-- https://github.com/brandonchinn178/toml-reader/issues/10
getFieldOr :: DecodeTOML a => Text -> a -> Decoder a
getFieldOr key def = fromMaybe def <$> getFieldOpt key

-- https://github.com/brandonchinn178/toml-reader/issues/11
applyDecoder :: Decoder a -> Value -> Decoder a
applyDecoder d v = makeDecoder $ \_ -> runDecoder d v

-- https://github.com/brandonchinn178/toml-reader/issues/12
getAllExcept :: [Text] -> Decoder (Map Text Value)
getAllExcept keys = (`Map.withoutKeys` Set.fromList keys) <$> tomlDecoder
