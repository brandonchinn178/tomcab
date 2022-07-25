{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Tomcab (
  runTomcab,
) where

import Control.Monad (when, (>=>))
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, takeFileName, (</>))
import TOML (TOMLError, decode, renderTOMLError)
import UnliftIO.Exception (Exception (..), fromEither, handleAny, throwIO)

import Tomcab.Cabal
import Tomcab.Render

runTomcab :: Maybe [FilePath] -> IO ()
runTomcab = \case
  Just files -> go files
  Nothing -> findCabalFiles >>= go
  where
    go = mapM_ $ \file ->
      handleAny (onError file) $ do
        pkg <- loadPackage file
        cabalPath <-
          case packageName pkg of
            Nothing -> throwIO MissingPackageName
            Just name -> pure $ takeDirectory file </> (Text.unpack name ++ ".cabal")
        Text.writeFile cabalPath (renderPackage pkg)

    onError file e = do
      when (isJust $ fromException @TomcabError e) $
        putStrLn $
          "tomcab failed to convert " ++ file ++ ":"
      putStrLn $ displayException e
      exitFailure

findCabalFiles :: IO [FilePath]
findCabalFiles = do
  cwd <- getCurrentDirectory
  filter ((== "package.toml") . takeFileName) <$> listDirectoryRecursive cwd

loadPackage :: FilePath -> IO Package
loadPackage fp = do
  pkg <- parsePackage =<< Text.readFile fp
  -- TODO: loadPackage all files mentioned in `extends`
  resolvePackage pkg

-- TODO: Remove ParseError: https://github.com/brandonchinn178/toml-reader/issues/13
parsePackage :: Text -> IO Package
parsePackage = fromEither . first ParseError . decode

{----- Package configuration resolution -----}

-- TODO: add "phase" of resolution in phantom type? a la Trees That Grow
resolvePackage :: Package -> IO Package
resolvePackage =
  foldr (>=>) pure $
    [ resolveDefaults
    , resolveAutoImports
    , resolveImports
    , resolveModules
    ]
  where
    resolveDefaults pkg = do
      let defaultTo f x = Just $ fromMaybe x (f pkg)
      pure
        pkg
          { packageCabalVersion = packageCabalVersion `defaultTo` "1.12"
          , packageBuildType = packageBuildType `defaultTo` "Simple"
          }

    resolveAutoImports pkg = do
      let addAutoImports info = info{packageImport = packageAutoImport pkg ++ packageImport info}
      pure
        pkg
          { packageAutoImport = []
          , packageLibraries = map (modifyBuildInfo addAutoImports) (packageLibraries pkg)
          , packageExecutables = map (modifyBuildInfo addAutoImports) (packageExecutables pkg)
          }

    resolveImports pkg = do
      let commonStanzas = packageCommonStanzas pkg
      packageLibraries' <- mapM (modifyBuildInfoM (mergeImports commonStanzas)) (packageLibraries pkg)
      packageExecutables' <- mapM (modifyBuildInfoM (mergeImports commonStanzas)) (packageExecutables pkg)
      pure
        pkg
          { packageCommonStanzas = Map.empty
          , packageLibraries = packageLibraries'
          , packageExecutables = packageExecutables'
          }

    resolveModules pkg = do
      let setOtherModules modules = modifyBuildInfo $ \info -> info{packageOtherModules = modules}
          resolveLibraryModules lib = do
            (exposed, other) <- resolveModulePatterns (packageExposedModules lib) lib
            pure $ setOtherModules other lib{packageExposedModules = exposed}
          resolveComponentModules parent = do
            (_, other) <- resolveModulePatterns [] parent
            pure $ setOtherModules other parent

      packageLibraries' <- mapM resolveLibraryModules (packageLibraries pkg)
      packageExecutables' <- mapM resolveComponentModules (packageExecutables pkg)
      pure
        pkg
          { packageLibraries = packageLibraries'
          , packageExecutables = packageExecutables'
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

data ModuleVisibility = Exposed | Other
  deriving (Eq)

resolveModulePatterns :: HasPackageBuildInfo a => [ModulePattern] -> a -> IO ([Module], [Module])
resolveModulePatterns exposedModules parent = do
  modules <- sort <$> concatMapM (listModules . Text.unpack) packageHsSourceDirs

  let patterns = map (,Exposed) exposedModules ++ map (,Other) packageOtherModules
  let matchedModules = zipMapMaybe (categorize patterns) modules
      extract x = map fst . filter ((== x) . snd)
  pure (extract Exposed matchedModules, extract Other matchedModules)
  where
    PackageBuildInfo{packageHsSourceDirs, packageOtherModules} = getBuildInfo parent

    concatMapM f = fmap concat . mapM f
    zipMapMaybe f = mapMaybe (\x -> (x,) <$> f x)

data ResolutionError
  = MissingPackageName
  | UnknownCommonStanza Text
  deriving (Show)

instance Exception ResolutionError where
  displayException = \case
    MissingPackageName -> "Package name is not specified"
    UnknownCommonStanza name -> "Unknown common stanza: " ++ Text.unpack name

{----- Errors -----}

data TomcabError
  = ParseError TOMLError
  | ResolutionError ResolutionError
  deriving (Show)

instance Exception TomcabError where
  fromException e =
    asum
      [ fromException e -- https://github.com/brandonchinn178/toml-reader/issues/13
      , ResolutionError <$> fromException e
      ]
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e
    ResolutionError e -> displayException e

{----- FilePath helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = fmap concat . mapM (go . (dir </>)) =<< listDirectory dir
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]

listModules :: FilePath -> IO [Module]
listModules dir = mapMaybe toModule <$> listDirectoryRecursive dir
  where
    toModule fp =
      case splitExtensions $ makeRelative dir fp of
        (file, ".hs") -> Just $ Module $ Text.splitOn "/" $ Text.pack file
        _ -> Nothing
