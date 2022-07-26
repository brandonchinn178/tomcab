{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Tomcab.Cabal.Module (
  -- * Module pattern
  ModulePath,
  ModulePattern (..),
  lookupPatternMatch,

  -- * Resolved module
  Module (..),
) where

import Control.Monad (forM_)
import Data.List (find, isPrefixOf, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import TOML (DecodeTOML (..), invalidValue, makeDecoder)

import Tomcab.Resolve.Phases (ResolutionPhase (..))

type family ModulePath (phase :: ResolutionPhase) where
  ModulePath 'Parsed = ModulePattern
  ModulePath 'ResolvedOptionals = ModulePattern
  ModulePath 'NoAutoImports = ModulePattern
  ModulePath 'NoImports = ModulePattern
  ModulePath _ = Module

data ModulePattern = ModulePattern
  { modulePath :: [Text]
  , modulePatternHasWildcard :: Bool
  -- ^ Does the pattern end with a wildcard?
  }
  deriving (Show)

instance DecodeTOML ModulePattern where
  tomlDecoder = tomlDecoder >>= decodePattern
    where
      decodePattern s = maybe badPattern pure $ do
        path <- NonEmpty.nonEmpty $ Text.splitOn "." s

        let (modulePath, modulePatternHasWildcard) =
              case unsnoc path of
                (path', "*") -> (path', True)
                _ -> (NonEmpty.toList path, False)

        forM_ modulePath $ \case
          "*" -> Nothing
          _ -> pure ()

        pure ModulePattern{..}

      badPattern = makeDecoder $ invalidValue "Invalid ModulePattern"
      unsnoc xs = (NonEmpty.init xs, NonEmpty.last xs)

newtype Module = Module [Text]
  deriving (Show, Eq, Ord)

-- | Return the annotation of the closest matching pattern for the given module.
lookupPatternMatch :: Module -> [(ModulePattern, a)] -> Maybe a
lookupPatternMatch (Module modl) = fmap snd . find isMatch . sortByPathDesc . filter isPossibleMatch
  where
    sortByPathDesc = sortOn (Down . length . modulePath . fst)
    isPossibleMatch = (`isPrefixOf` modl) . modulePath . fst
    isMatch (ModulePattern{..}, _) = modulePatternHasWildcard || modulePath == modl
