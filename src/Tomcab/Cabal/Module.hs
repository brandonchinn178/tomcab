{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tomcab.Cabal.Module (
  ParsedModulePattern (..),
  ModulePattern,
  pattern ModulePattern,
  Module,
  pattern Module,
  parsePattern,
  renderPattern,
  categorize,
) where

import Control.Monad (forM_)
import Data.List (find, isPrefixOf, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import TOML (DecodeTOML (..))

data ParsedModulePattern = ParsedModulePattern
  { patternPath :: [Text]
  , patternHasWildcard :: Bool
  -- ^ Does the pattern end with a wildcard?
  }
  deriving (Show, Eq, Ord)

-- TODO: make a separate type
type ModulePattern = ParsedModulePattern
pattern ModulePattern :: Text -> ParsedModulePattern
pattern ModulePattern path = ParsedModulePattern [path] False
{-# COMPLETE ModulePattern #-}

instance DecodeTOML ModulePattern where
  tomlDecoder = ModulePattern <$> tomlDecoder

-- TODO: make a separate type
-- invariant: patternHasWildcard is False
type Module = ModulePattern
pattern Module :: [Text] -> ParsedModulePattern
pattern Module path = ParsedModulePattern path False
{-# COMPLETE Module #-}

parsePattern :: ModulePattern -> Maybe ParsedModulePattern
parsePattern (ModulePattern path) = do
  pathParts <- NonEmpty.nonEmpty $ Text.splitOn "." path

  let (patternPath, patternHasWildcard) =
        case unsnoc pathParts of
          (pathParts', "*") -> (pathParts', True)
          _ -> (NonEmpty.toList pathParts, False)

  forM_ patternPath $ \case
    "*" -> Nothing
    _ -> pure ()

  pure ParsedModulePattern{..}
  where
    unsnoc xs = (NonEmpty.init xs, NonEmpty.last xs)

renderPattern :: ParsedModulePattern -> Text
renderPattern ParsedModulePattern{..} =
  Text.concat
    [ Text.intercalate "." patternPath
    , if patternHasWildcard then ".*" else ""
    ]

-- | Return the annotation of the closest matching pattern for the given module.
categorize :: [(ParsedModulePattern, a)] -> Module -> Maybe a
categorize patterns (Module modl) =
  fmap snd . find isMatch . sortByPathDesc . filter isPossibleMatch $ patterns
  where
    sortByPathDesc = sortOn (Down . length . patternPath . fst)
    isPossibleMatch = (`isPrefixOf` modl) . patternPath . fst
    isMatch (ParsedModulePattern{..}, _) = patternHasWildcard || patternPath == modl
