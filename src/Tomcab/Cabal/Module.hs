{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tomcab.Cabal.Module (
  -- * Module pattern
  ModulePattern (..),
  categorize,

  -- * Resolved module
  Module,
  pattern Module,
) where

import Control.Monad (forM_)
import Data.List (find, isPrefixOf, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import TOML (DecodeTOML (..), invalidValue, makeDecoder)

data ModulePattern = ModulePattern
  { patternPath :: [Text]
  , patternHasWildcard :: Bool
  -- ^ Does the pattern end with a wildcard?
  }
  deriving (Show, Eq, Ord)

instance DecodeTOML ModulePattern where
  tomlDecoder = tomlDecoder >>= decodePattern
    where
      decodePattern s = maybe badPattern pure $ do
        path <- NonEmpty.nonEmpty $ Text.splitOn "." s

        let (patternPath, patternHasWildcard) =
              case unsnoc path of
                (path', "*") -> (path', True)
                _ -> (NonEmpty.toList path, False)

        forM_ patternPath $ \case
          "*" -> Nothing
          _ -> pure ()

        pure ModulePattern{..}

      badPattern = makeDecoder $ invalidValue "Invalid ModulePattern"
      unsnoc xs = (NonEmpty.init xs, NonEmpty.last xs)

-- TODO: make a separate type
-- invariant: patternHasWildcard is False
type Module = ModulePattern
pattern Module :: [Text] -> ModulePattern
pattern Module path = ModulePattern path False
{-# COMPLETE Module #-}

-- | Return the annotation of the closest matching pattern for the given module.
categorize :: [(ModulePattern, a)] -> Module -> Maybe a
categorize patterns (Module modl) =
  fmap snd . find isMatch . sortByPathDesc . filter isPossibleMatch $ patterns
  where
    sortByPathDesc = sortOn (Down . length . patternPath . fst)
    isPossibleMatch = (`isPrefixOf` modl) . patternPath . fst
    isMatch (ModulePattern{..}, _) = patternHasWildcard || patternPath == modl
