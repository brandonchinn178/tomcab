{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Tomcab.Resolve (
  resolvePackage,
  ResolutionError (..),
) where

import Control.Monad ((<=<), (>=>))
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (makeRelative, splitExtensions)
import UnliftIO.Exception (Exception (..), fromEither, throwIO)

import Tomcab.Cabal (
  CabalValue (..),
  CommonStanza (..),
  CommonStanzas,
  Conditional (..),
  ImportField,
  Module (..),
  ModulePath,
  ModulePattern (..),
  Package (..),
  PackageBuildInfo (..),
  PackageExecutable (..),
  PackageLibrary (..),
  PackageTestSuite (..),
  TestTypeField,
 )
import Tomcab.Cabal.Module (lookupPatternMatch, moduleToPattern, patternToModule)
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
      , packageTestSuites = map resolveTestName packageTestSuites
      , ..
      }
  where
    resolveTestName PackageTestSuite{..} =
      PackageTestSuite
        { packageTestType = fromMaybe "exitcode-stdio-1.0" packageTestType
        , packageTestInfo = coerceInfoWith resolveTestName packageTestInfo
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
      , packageTestSuites = map (coerceTest . modifyBuildInfo addAutoImports) packageTestSuites
      , ..
      }
  where
    addAutoImports :: PackageBuildInfo PreResolveAutoImports parent -> PackageBuildInfo PreResolveAutoImports parent
    addAutoImports info = info{packageImport = packageAutoImport ++ packageImport info}

{----- ResolveImports -----}

resolveImports :: Package PreResolveImports -> IO (Package PostResolveImports)
resolveImports Package{..} = do
  packageLibraries' <- fromEither $ mapM resolve packageLibraries
  packageExecutables' <- fromEither $ mapM resolve packageExecutables
  packageTestSuites' <- fromEither $ mapM resolve packageTestSuites
  pure
    Package
      { packageCommonStanzas = unset
      , packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , packageTestSuites = packageTestSuites'
      , ..
      }
  where
    resolve ::
      ( HasPackageBuildInfo parent
      , CanResolveImports parent
      , FromCommonStanza PreResolveImports parent
      ) =>
      parent PreResolveImports ->
      ResolveM (parent PostResolveImports)
    resolve parent = do
      info <- mergeImports packageCommonStanzas resolve (getBuildInfo parent)
      pure $ setResolvedImports info parent

class CanResolveImports parent where
  setResolvedImports ::
    PackageBuildInfo PostResolveImports parent ->
    parent PreResolveImports ->
    parent PostResolveImports
instance CanResolveImports PackageLibrary where
  setResolvedImports info PackageLibrary{..} = PackageLibrary{packageLibraryInfo = info, ..}
instance CanResolveImports PackageExecutable where
  setResolvedImports info PackageExecutable{..} = PackageExecutable{packageExeInfo = info, ..}
instance CanResolveImports PackageTestSuite where
  setResolvedImports info PackageTestSuite{..} = PackageTestSuite{packageTestInfo = info, ..}

mergeImports ::
  FromCommonStanza PreResolveImports parent =>
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
  FromCommonStanza PreResolveImports parent =>
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
    , packageInfoFields = Map.unionWith mergeField (packageInfoFields commonInfo) (packageInfoFields info)
    }
  where
    upcastConditional cond =
      cond
        { conditionThen = fromCommonStanza (conditionThen cond)
        , conditionElif = map (fmap fromCommonStanza) (conditionElif cond)
        , conditionElse = fmap fromCommonStanza (conditionElse cond)
        }

    mergeField commonField infoField =
      case (commonField, infoField) of
        (CabalListValue as, CabalListValue bs) -> CabalListValue (as <> bs)
        _ -> infoField

{----- ResolveModules -----}

resolveModules :: Package PreResolveModules -> IO (Package PostResolveModules)
resolveModules Package{..} = do
  packageLibraries' <- mapM resolve packageLibraries
  packageExecutables' <- mapM resolve packageExecutables
  packageTestSuites' <- mapM resolve packageTestSuites
  pure
    Package
      { packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , packageTestSuites = packageTestSuites'
      , ..
      }
  where
    resolve ::
      ( HasPackageBuildInfo parent
      , CanResolveModules parent
      ) =>
      parent PreResolveModules ->
      IO (parent PostResolveModules)
    resolve parent = do
      let info = getBuildInfo parent
          exposedModulesPatterns = getExposedModules parent
          otherModulesPatterns = packageOtherModules info
          ignoredPatterns = getIgnoredModules parent

      let (exposedModulesExplicit, exposedModulesNonExplicit) = extractExplicitModules exposedModulesPatterns
          (otherModulesExplicit, otherModulesNonExplicit) = extractExplicitModules otherModulesPatterns

      (exposedModulesFound, otherModulesFound) <-
        if null exposedModulesNonExplicit && null otherModulesNonExplicit
          then -- if everything's explicit, don't bother listing modules + pattern matching
            pure ([], [])
          else do
            modules <- concatMapM (listModules . Text.unpack) (packageHsSourceDirs info)
            pure $ resolveModulePatterns (exposedModulesPatterns, otherModulesPatterns, ignoredPatterns) modules

      let exposedModules = nubSort $ exposedModulesExplicit ++ exposedModulesFound
          otherModules = nubSort $ otherModulesExplicit ++ otherModulesFound
      info' <- resolveInfo otherModules info
      pure $ setResolvedModules exposedModules info' parent

    resolveInfo ::
      ( HasPackageBuildInfo parent
      , CanResolveModules parent
      ) =>
      [Module] ->
      PackageBuildInfo PreResolveModules parent ->
      IO (PackageBuildInfo PostResolveModules parent)
    resolveInfo otherModules PackageBuildInfo{..} = do
      packageInfoIfs' <- mapM (traverse resolve) packageInfoIfs
      pure
        PackageBuildInfo
          { packageOtherModules = otherModules
          , packageInfoIfs = packageInfoIfs'
          , ..
          }

    concatMapM f = fmap concat . mapM f
    nubSort = Set.toAscList . Set.fromList

class CanResolveModules parent where
  getExposedModules :: parent PreResolveModules -> [ModulePattern]
  getExposedModules _ = []

  getIgnoredModules :: parent PreResolveModules -> [ModulePattern]
  getIgnoredModules _ = []

  setResolvedModules ::
    [Module] ->
    PackageBuildInfo PostResolveModules parent ->
    parent PreResolveModules ->
    parent PostResolveModules
instance CanResolveModules PackageLibrary where
  getExposedModules = packageExposedModules
  setResolvedModules exposedModules info PackageLibrary{..} =
    PackageLibrary
      { packageExposedModules = exposedModules
      , packageLibraryInfo = info
      , ..
      }
instance CanResolveModules PackageExecutable where
  getIgnoredModules = maybeToList . (toModulePattern <=< packageExeMainIs)
  setResolvedModules _ info PackageExecutable{..} = PackageExecutable{packageExeInfo = info, ..}
instance CanResolveModules PackageTestSuite where
  getIgnoredModules = maybeToList . (toModulePattern <=< packageTestMainIs)
  setResolvedModules _ info PackageTestSuite{..} = PackageTestSuite{packageTestInfo = info, ..}

extractExplicitModules :: [ModulePattern] -> ([Module], [ModulePattern])
extractExplicitModules = partitionMaybes patternToModule
  where
    partitionMaybes f = partitionEithers . map (\x -> maybe (Right x) Left (f x))

data ModulePatternType = ExposedPattern | OtherPattern | IgnoredPattern
  deriving (Eq)

resolveModulePatterns :: ([ModulePattern], [ModulePattern], [ModulePattern]) -> [Module] -> ([Module], [Module])
resolveModulePatterns (exposedModules, otherModules, ignoreModules) modules =
  fromMatchResults $
    flip map modules $ \modl ->
      (modl, lookupPatternMatch modl patterns)
  where
    patterns =
      concat
        [ map (,ExposedPattern) exposedModules
        , map (,OtherPattern) otherModules
        , map (,IgnoredPattern) ignoreModules
        ]

    fromMatchResults = \case
      [] -> ([], [])
      (modl, patternType) : matches ->
        let (exposed, other) = fromMatchResults matches
         in case patternType of
              Nothing -> (exposed, other)
              Just ExposedPattern -> (modl : exposed, other)
              Just OtherPattern -> (exposed, modl : other)
              Just IgnoredPattern -> (exposed, other)

listModules :: FilePath -> IO [Module]
listModules dir = mapMaybe (toModule . makeRelative dir) <$> listDirectoryRecursive dir

toModule :: FilePath -> Maybe Module
toModule fp =
  case splitExtensions fp of
    (file, ".hs") -> Just $ Module $ Text.splitOn "/" $ Text.pack file
    _ -> Nothing

toModulePattern :: Text -> Maybe ModulePattern
toModulePattern = fmap moduleToPattern . toModule . Text.unpack

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

type CoerciblePackageTestSuite phase1 phase2 =
  ( CoerciblePackageBuildInfo phase1 phase2
  , TestTypeField phase1 ~ TestTypeField phase2
  )

coerceTest ::
  CoerciblePackageTestSuite phase1 phase2 =>
  PackageTestSuite phase1 ->
  PackageTestSuite phase2
coerceTest PackageTestSuite{..} =
  PackageTestSuite
    { packageTestInfo = coerceInfoWith coerceTest packageTestInfo
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
  ( ImportField phase1 ~ ImportField phase2
  , ModulePath phase1 ~ ModulePath phase2
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

class HasPackageBuildInfo parent where
  getBuildInfo :: parent phase -> PackageBuildInfo phase parent
  modifyBuildInfo :: (PackageBuildInfo phase parent -> PackageBuildInfo phase parent) -> (parent phase -> parent phase)

instance HasPackageBuildInfo PackageLibrary where
  getBuildInfo = packageLibraryInfo
  modifyBuildInfo f lib = lib{packageLibraryInfo = f (packageLibraryInfo lib)}

instance HasPackageBuildInfo PackageExecutable where
  getBuildInfo = packageExeInfo
  modifyBuildInfo f exe = exe{packageExeInfo = f (packageExeInfo exe)}

instance HasPackageBuildInfo PackageTestSuite where
  getBuildInfo = packageTestInfo
  modifyBuildInfo f test = test{packageTestInfo = f (packageTestInfo test)}

class FromCommonStanza phase parent where
  fromCommonStanza :: CommonStanza phase -> parent phase

instance FromCommonStanza phase PackageLibrary where
  fromCommonStanza (CommonStanza info) =
    PackageLibrary
      mempty
      mempty
      (mapPackageBuildInfo fromCommonStanza info)

instance FromCommonStanza phase PackageExecutable where
  fromCommonStanza (CommonStanza info) =
    PackageExecutable
      mempty
      mempty
      (mapPackageBuildInfo fromCommonStanza info)

instance FromCommonStanza PreResolveImports PackageTestSuite where
  fromCommonStanza (CommonStanza info) =
    PackageTestSuite
      mempty
      mempty
      mempty
      (mapPackageBuildInfo fromCommonStanza info)

mapPackageBuildInfo :: (parent1 phase -> parent2 phase) -> PackageBuildInfo phase parent1 -> PackageBuildInfo phase parent2
mapPackageBuildInfo f info =
  info
    { packageInfoIfs = map (fmap f) (packageInfoIfs info)
    }
