{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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
import UnliftIO.Exception (Exception (..), fromEither, throwIO)

import Tomcab.Cabal (
  CommonStanza (..),
  CommonStanzas,
  Conditional (..),
  Module,
  ModulePattern,
  Package (..),
  PackageBuildInfo (..),
  PackageExecutable (..),
  PackageLibrary (..),
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
      , packageCommonStanzas = fmap coerceCommonStanza packageCommonStanzas
      , packageLibraries = map coerceLib packageLibraries
      , packageExecutables = map coerceExe packageExecutables
      , ..
      }

{----- ResolveAutoImports -----}

resolveAutoImports :: Package PreResolveAutoImports -> IO (Package PostResolveAutoImports)
resolveAutoImports Package{..} =
  pure
    Package
      { packageAutoImport = unset
      , packageCommonStanzas = fmap coerceCommonStanza packageCommonStanzas
      , packageLibraries = map (coerceLib . modifyBuildInfo addAutoImports) packageLibraries
      , packageExecutables = map (coerceExe . modifyBuildInfo addAutoImports) packageExecutables
      , ..
      }
  where
    addAutoImports :: PackageBuildInfo PreResolveAutoImports parent -> PackageBuildInfo PreResolveAutoImports parent
    addAutoImports info = info{packageImport = packageAutoImport ++ packageImport info}

{----- ResolveImports -----}

resolveImports :: Package PreResolveImports -> IO (Package PostResolveImports)
resolveImports Package{..} = do
  packageLibraries' <- fromEither $ mapM resolveLib packageLibraries
  packageExecutables' <- fromEither $ mapM resolveExe packageExecutables
  pure
    Package
      { packageCommonStanzas = unset
      , packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }
  where
    resolveLib PackageLibrary{..} = do
      packageLibraryInfo' <- mergeImports packageCommonStanzas resolveLib packageLibraryInfo
      pure PackageLibrary{packageLibraryInfo = packageLibraryInfo', ..}

    resolveExe PackageExecutable{..} = do
      packageExeInfo' <- mergeImports packageCommonStanzas resolveExe packageExeInfo
      pure PackageExecutable{packageExeInfo = packageExeInfo', ..}

mergeImports ::
  (HasPackageBuildInfo parent, FromCommonStanza parent) =>
  CommonStanzas PreResolveImports ->
  (parent PreResolveImports -> ResolveM (parent PostResolveImports)) ->
  PackageBuildInfo PreResolveImports parent ->
  ResolveM (PackageBuildInfo PostResolveImports parent)
mergeImports commonStanzas resolveParent info0 = go (packageImport info0) info0
  where
    go (imp : imps) info =
      case imp `Map.lookup` commonStanzas of
        Nothing -> Left $ UnknownCommonStanza imp
        Just commonStanza ->
          go
            (packageImport (commonStanzaInfo commonStanza) ++ imps)
            (mergeCommonStanza commonStanza info)
    go [] PackageBuildInfo{..} = do
      packageInfoIfs' <- mapM (traverse resolveParent) packageInfoIfs
      pure
        PackageBuildInfo
          { packageImport = unset
          , packageInfoIfs = packageInfoIfs'
          , ..
          }

mergeCommonStanza ::
  (HasPackageBuildInfo parent, FromCommonStanza parent) =>
  CommonStanza PreResolveImports ->
  PackageBuildInfo PreResolveImports parent ->
  PackageBuildInfo PreResolveImports parent
mergeCommonStanza (CommonStanza commonInfo) info =
  PackageBuildInfo
    { packageImport = packageImport commonInfo <> packageImport info
    , packageBuildDepends = packageBuildDepends commonInfo <> packageBuildDepends info
    , packageOtherModules = packageOtherModules commonInfo <> packageOtherModules info
    , packageHsSourceDirs = packageHsSourceDirs commonInfo <> packageHsSourceDirs info
    , packageInfoIfs = map upcastConditional (packageInfoIfs commonInfo) <> packageInfoIfs info
    , -- union is left-biased; info fields should override commonInfo fields
      packageInfoFields = packageInfoFields info `Map.union` packageInfoFields commonInfo
    }
  where
    upcastConditional cond =
      cond
        { conditionThen = fromCommonStanza (conditionThen cond)
        , conditionElif = map (fmap fromCommonStanza) (conditionElif cond)
        , conditionElse = fmap fromCommonStanza (conditionElse cond)
        }

{----- ResolveModules -----}

resolveModules :: Package PreResolveModules -> IO (Package PostResolveModules)
resolveModules Package{..} = do
  packageLibraries' <- mapM resolveLib packageLibraries
  packageExecutables' <- mapM resolveExe packageExecutables
  pure
    Package
      { packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }
  where
    setOtherModules modules = modifyBuildInfo $ \info -> info{packageOtherModules = modules}

    resolveLib lib = do
      (exposed, other) <- resolveModulePatterns (packageExposedModules lib) lib
      pure . coerceLib $ setOtherModules other lib{packageExposedModules = exposed}

    resolveExe = fmap coerceExe . resolveComponent

    resolveComponent parent = do
      (_, other) <- resolveModulePatterns [] parent
      pure $ setOtherModules other parent

data ModuleVisibility = Exposed | Other
  deriving (Eq)

resolveModulePatterns :: HasPackageBuildInfo parent => [ModulePattern] -> parent PreResolveModules -> IO ([Module], [Module])
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

type ResolveM a = Either ResolutionError a

data ResolutionError
  = MissingPackageName
  | UnknownCommonStanza Text
  deriving (Show)

instance Exception ResolutionError where
  displayException = \case
    MissingPackageName -> "Package name is not specified"
    UnknownCommonStanza name -> "Unknown common stanza: " ++ Text.unpack name

{----- Coerce helpers -----}

coerceLib ::
  CoerciblePackageBuildInfo phase1 phase2 =>
  PackageLibrary phase1 ->
  PackageLibrary phase2
coerceLib PackageLibrary{..} =
  PackageLibrary
    { packageLibraryInfo = coerceInfoWith coerceLib packageLibraryInfo
    , ..
    }

coerceExe ::
  CoerciblePackageBuildInfo phase1 phase2 =>
  PackageExecutable phase1 ->
  PackageExecutable phase2
coerceExe PackageExecutable{..} =
  PackageExecutable
    { packageExeInfo = coerceInfoWith coerceExe packageExeInfo
    , ..
    }

coerceCommonStanza ::
  CoerciblePackageBuildInfo phase1 phase2 =>
  CommonStanza phase1 ->
  CommonStanza phase2
coerceCommonStanza CommonStanza{..} =
  CommonStanza
    { commonStanzaInfo = coerceInfoWith coerceCommonStanza commonStanzaInfo
    }

type CoerciblePackageBuildInfo phase1 phase2 =
  ( UnsetFrom 'NoImports phase1 [Text] ~ UnsetFrom 'NoImports phase2 [Text]
  )

coerceInfoWith ::
  CoerciblePackageBuildInfo phase1 phase2 =>
  (parent phase1 -> parent phase2) ->
  PackageBuildInfo phase1 parent ->
  PackageBuildInfo phase2 parent
coerceInfoWith coerceParent PackageBuildInfo{..} =
  PackageBuildInfo
    { packageInfoIfs = map (fmap coerceParent) packageInfoIfs
    , ..
    }

{----- PackageBuildInfo + CommonStanza helpers -----}

class FromCommonStanza parent where
  fromCommonStanza :: CommonStanza phase -> parent phase

class HasPackageBuildInfo parent where
  getBuildInfo :: parent phase -> PackageBuildInfo phase parent

  modifyBuildInfo :: (PackageBuildInfo phase parent -> PackageBuildInfo phase parent) -> (parent phase -> parent phase)

instance HasPackageBuildInfo PackageLibrary where
  getBuildInfo = packageLibraryInfo
  modifyBuildInfo f lib = lib{packageLibraryInfo = f (packageLibraryInfo lib)}

instance FromCommonStanza PackageLibrary where
  fromCommonStanza (CommonStanza info) =
    PackageLibrary
      mempty
      mempty
      (mapPackageBuildInfo fromCommonStanza info)

instance HasPackageBuildInfo PackageExecutable where
  getBuildInfo = packageExeInfo
  modifyBuildInfo f exe = exe{packageExeInfo = f (packageExeInfo exe)}

instance FromCommonStanza PackageExecutable where
  fromCommonStanza (CommonStanza info) =
    PackageExecutable
      mempty
      (mapPackageBuildInfo fromCommonStanza info)

mapPackageBuildInfo :: (parent1 phase -> parent2 phase) -> PackageBuildInfo phase parent1 -> PackageBuildInfo phase parent2
mapPackageBuildInfo f info =
  info
    { packageInfoIfs = map (fmap f) (packageInfoIfs info)
    }
