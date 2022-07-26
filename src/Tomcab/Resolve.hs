{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Tomcab.Resolve (
  resolvePackage,
  ResolutionError (..),
) where

import Control.Monad ((>=>))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (makeRelative, splitExtensions)
import UnliftIO.Exception (Exception (..), throwIO)

import Tomcab.Cabal (
  CommonStanza (..),
  CommonStanzas,
  FromCommonStanza,
  HasPackageBuildInfo,
  Module,
  ModulePattern,
  Package (..),
  PackageBuildInfo (..),
  PackageLibrary (..),
  getBuildInfo,
  mergeCommonStanza,
  modifyBuildInfo,
  modifyBuildInfoM,
  pattern Module,
 )
import Tomcab.Cabal.Module (lookupPatternMatch)
import Tomcab.Resolve.Phases
import Tomcab.Utils.FilePath (listDirectoryRecursive)

resolvePackage :: Package Unresolved -> IO (Package Resolved)
resolvePackage =
  resolveOptionals
    >=> resolveAutoImports
    >=> resolveImports
    >=> resolveModules

type PreResolveOptionals = 'Parsed
type PostResolveOptionals = 'ResolvedOptionals

type PreResolveAutoImports = 'ResolvedOptionals
type PostResolveAutoImports = 'NoAutoImports

type PreResolveImports = 'NoAutoImports
type PostResolveImports = 'NoImports

type PreResolveModules = 'NoImports
type PostResolveModules = 'NoModulePatterns

{----- ResolveOptionals -----}

resolveOptionals :: Package PreResolveOptionals -> IO (Package PostResolveOptionals)
resolveOptionals Package{..} = do
  packageName' <- maybe (throwIO MissingPackageName) pure packageName
  pure
    Package
      { packageName = packageName'
      , packageCabalVersion = fromMaybe "1.12" packageCabalVersion
      , packageBuildType = fromMaybe "Simple" packageBuildType
      , ..
      }

{----- ResolveAutoImports -----}

resolveAutoImports :: Package PreResolveAutoImports -> IO (Package PostResolveAutoImports)
resolveAutoImports Package{..} = do
  let addAutoImports info = info{packageImport = packageAutoImport ++ packageImport info}
  pure
    Package
      { packageAutoImport = []
      , packageLibraries = map (modifyBuildInfo addAutoImports) packageLibraries
      , packageExecutables = map (modifyBuildInfo addAutoImports) packageExecutables
      , ..
      }

{----- ResolveImports -----}

resolveImports :: Package PreResolveImports -> IO (Package PostResolveImports)
resolveImports Package{..} = do
  let commonStanzas = packageCommonStanzas
  packageLibraries' <- mapM (modifyBuildInfoM (mergeImports commonStanzas)) packageLibraries
  packageExecutables' <- mapM (modifyBuildInfoM (mergeImports commonStanzas)) packageExecutables
  pure
    Package
      { packageCommonStanzas = Map.empty
      , packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }

mergeImports ::
  (HasPackageBuildInfo a, FromCommonStanza a) =>
  CommonStanzas ->
  PackageBuildInfo a ->
  IO (PackageBuildInfo a)
mergeImports commonStanzas info0 = go (packageImport info0) info0
  where
    go [] info = pure info{packageImport = []}
    go (imp : imps) info =
      case imp `Map.lookup` commonStanzas of
        Nothing -> throwIO $ UnknownCommonStanza imp
        Just commonStanza ->
          go
            (packageImport (commonStanzaInfo commonStanza) ++ imps)
            (mergeCommonStanza commonStanza info)

{----- ResolveModules -----}

resolveModules :: Package PreResolveModules -> IO (Package PostResolveModules)
resolveModules Package{..} = do
  let setOtherModules modules = modifyBuildInfo $ \info -> info{packageOtherModules = modules}
      resolveLibraryModules lib = do
        (exposed, other) <- resolveModulePatterns (packageExposedModules lib) lib
        pure $ setOtherModules other lib{packageExposedModules = exposed}
      resolveComponentModules parent = do
        (_, other) <- resolveModulePatterns [] parent
        pure $ setOtherModules other parent

  packageLibraries' <- mapM resolveLibraryModules packageLibraries
  packageExecutables' <- mapM resolveComponentModules packageExecutables
  pure
    Package
      { packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }

data ModuleVisibility = Exposed | Other
  deriving (Eq)

resolveModulePatterns :: HasPackageBuildInfo a => [ModulePattern] -> a -> IO ([Module], [Module])
resolveModulePatterns exposedModules parent = do
  modules <- sort <$> concatMapM (listModules . Text.unpack) packageHsSourceDirs

  let patterns = map (,Exposed) exposedModules ++ map (,Other) packageOtherModules
  let matchedModules = zipMapMaybe (`lookupPatternMatch` patterns) modules
      extract x = map fst . filter ((== x) . snd)
  pure (extract Exposed matchedModules, extract Other matchedModules)
  where
    PackageBuildInfo{packageHsSourceDirs, packageOtherModules} = getBuildInfo parent

    concatMapM f = fmap concat . mapM f
    zipMapMaybe f = mapMaybe (\x -> (x,) <$> f x)

listModules :: FilePath -> IO [Module]
listModules dir = mapMaybe toModule <$> listDirectoryRecursive dir
  where
    toModule fp =
      case splitExtensions $ makeRelative dir fp of
        (file, ".hs") -> Just $ Module $ Text.splitOn "/" $ Text.pack file
        _ -> Nothing

{----- Errors -----}

data ResolutionError
  = MissingPackageName
  | UnknownCommonStanza Text
  deriving (Show)

instance Exception ResolutionError where
  displayException = \case
    MissingPackageName -> "Package name is not specified"
    UnknownCommonStanza name -> "Unknown common stanza: " ++ Text.unpack name
