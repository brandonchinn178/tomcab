{-# LANGUAGE AllowAmbiguousTypes #-}
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
  FromCommonStanza,
  HasPackageBuildInfo,
  Module,
  ModulePattern,
  Package (..),
  PackageBuildInfo (..),
  PackageExecutable (..),
  PackageLibrary (..),
  fromCommonStanza,
  getBuildInfo,
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
      , packageCommonStanzas = transitionCommon <$> packageCommonStanzas
      , packageLibraries = transitionLib <$> packageLibraries
      , packageExecutables = transitionExe <$> packageExecutables
      , ..
      }
  where
    transitionCommon :: CommonStanza PreResolveOptionals -> CommonStanza PostResolveOptionals
    transitionCommon CommonStanza{..} =
      CommonStanza
        { commonStanzaInfo = transitionInfo transitionCommon commonStanzaInfo
        }

    transitionLib :: PackageLibrary PreResolveOptionals -> PackageLibrary PostResolveOptionals
    transitionLib PackageLibrary{..} =
      PackageLibrary
        { packageLibraryInfo = transitionInfo transitionLib packageLibraryInfo
        , ..
        }

    transitionExe :: PackageExecutable PreResolveOptionals -> PackageExecutable PostResolveOptionals
    transitionExe PackageExecutable{..} =
      PackageExecutable
        { packageExeInfo = transitionInfo transitionExe packageExeInfo
        , ..
        }

    transitionInfo ::
      (parent PreResolveOptionals -> parent PostResolveOptionals) ->
      PackageBuildInfo PreResolveOptionals parent ->
      PackageBuildInfo PostResolveOptionals parent
    transitionInfo transitionParent PackageBuildInfo{..} =
      PackageBuildInfo
        { packageInfoIfs = fmap transitionParent <$> packageInfoIfs
        , ..
        }

{----- ResolveAutoImports -----}

resolveAutoImports :: Package PreResolveAutoImports -> IO (Package PostResolveAutoImports)
resolveAutoImports Package{..} =
  pure
    Package
      { packageAutoImport = []
      , packageCommonStanzas = transitionCommon <$> packageCommonStanzas
      , packageLibraries = transitionLib . modifyBuildInfo addAutoImports <$> packageLibraries
      , packageExecutables = transitionExe . modifyBuildInfo addAutoImports <$> packageExecutables
      , ..
      }
  where
    addAutoImports :: PackageBuildInfo PreResolveAutoImports parent -> PackageBuildInfo PreResolveAutoImports parent
    addAutoImports info = info{packageImport = packageAutoImport ++ packageImport info}

    transitionCommon :: CommonStanza PreResolveAutoImports -> CommonStanza PostResolveAutoImports
    transitionCommon CommonStanza{..} =
      CommonStanza
        { commonStanzaInfo = transitionInfo transitionCommon commonStanzaInfo
        }

    transitionLib :: PackageLibrary PreResolveAutoImports -> PackageLibrary PostResolveAutoImports
    transitionLib PackageLibrary{..} =
      PackageLibrary
        { packageLibraryInfo = transitionInfo transitionLib packageLibraryInfo
        , ..
        }

    transitionExe :: PackageExecutable PreResolveAutoImports -> PackageExecutable PostResolveAutoImports
    transitionExe PackageExecutable{..} =
      PackageExecutable
        { packageExeInfo = transitionInfo transitionExe packageExeInfo
        , ..
        }

    transitionInfo ::
      (parent PreResolveAutoImports -> parent PostResolveAutoImports) ->
      PackageBuildInfo PreResolveAutoImports parent ->
      PackageBuildInfo PostResolveAutoImports parent
    transitionInfo transitionParent PackageBuildInfo{..} =
      PackageBuildInfo
        { packageInfoIfs = fmap transitionParent <$> packageInfoIfs
        , ..
        }

{----- ResolveImports -----}

resolveImports :: Package PreResolveImports -> IO (Package PostResolveImports)
resolveImports Package{..} = do
  packageLibraries' <- fromEither $ mapM resolveLib packageLibraries
  packageExecutables' <- fromEither $ mapM resolveExe packageExecutables
  pure
    Package
      { packageCommonStanzas = Map.empty
      , packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }
  where
    resolveLib :: PackageLibrary PreResolveImports -> ResolveM (PackageLibrary PostResolveImports)
    resolveLib = modifyBuildInfoM (mergeImports packageCommonStanzas resolveLib)

    resolveExe :: PackageExecutable PreResolveImports -> ResolveM (PackageExecutable PostResolveImports)
    resolveExe = modifyBuildInfoM (mergeImports packageCommonStanzas resolveExe)

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
          { packageImport = []
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
      { packageCommonStanzas = Map.empty
      , packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }
  where
    setOtherModules transitionParent modules =
      modifyBuildInfo $ \info -> transitionInfo transitionParent info{packageOtherModules = modules}

    resolveLib lib = do
      (exposed, other) <- resolveModulePatterns (packageExposedModules lib) lib
      pure $ setOtherModules transitionLib other lib{packageExposedModules = exposed}

    resolveExe = resolveComponent transitionExe

    resolveComponent transitionParent parent = do
      (_, other) <- resolveModulePatterns [] parent
      pure $ setOtherModules transitionParent other parent

    transitionLib :: PackageLibrary PreResolveModules -> PackageLibrary PostResolveModules
    transitionLib PackageLibrary{..} =
      PackageLibrary
        { packageLibraryInfo = transitionInfo transitionLib packageLibraryInfo
        , ..
        }

    transitionExe :: PackageExecutable PreResolveModules -> PackageExecutable PostResolveModules
    transitionExe PackageExecutable{..} =
      PackageExecutable
        { packageExeInfo = transitionInfo transitionExe packageExeInfo
        , ..
        }

    transitionInfo ::
      (parent PreResolveModules -> parent PostResolveModules) ->
      PackageBuildInfo PreResolveModules parent ->
      PackageBuildInfo PostResolveModules parent
    transitionInfo transitionParent PackageBuildInfo{..} =
      PackageBuildInfo
        { packageInfoIfs = fmap transitionParent <$> packageInfoIfs
        , ..
        }

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
