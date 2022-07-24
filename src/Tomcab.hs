{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tomcab (
  runTomcab,
  generateCabalFile,
) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import TOML (DecodeTOML (..), TOMLError, Value, decode, getArrayOf, getField, getFieldOpt, getFieldWith, renderTOMLError)
import UnliftIO.Exception (Exception (..), fromEither, handleAny)

runTomcab :: Maybe [FilePath] -> IO ()
runTomcab = \case
  Just files -> go files
  Nothing -> findCabalFiles >>= go
  where
    go = mapM_ $ \file ->
      handleAny (onError file) $
        -- TODO
        generateCabalFile file >>= Text.putStrLn

    onError file e = do
      when (isJust $ fromException @TomcabError e) $
        putStrLn $ "tomcab failed to convert " ++ file ++ ":"
      putStrLn $ displayException e
      exitFailure

findCabalFiles :: IO [FilePath]
findCabalFiles = do
  cwd <- getCurrentDirectory
  filter ((== "package.toml") . takeFileName) <$> listDirectoryRecursive cwd

{----- FilePath helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap concat . mapM (go . (fp </>)) =<< listDirectory fp
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]

{----- TODO: move to another module -----}

generateCabalFile :: FilePath -> IO Text
generateCabalFile = fmap renderPackage . loadPackage

loadPackage :: FilePath -> IO Package
loadPackage fp = do
  pkg <- fromEither . parsePackage =<< Text.readFile fp
  -- TODO: loadPackage all files mentioned in `extends`
  fromEither $ resolvePackage pkg

data TomcabError
  = ParseError TOMLError
  deriving (Show)

instance Exception TomcabError where
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e

data Package = Package
  { packageName :: Maybe Text
  , packageVersion :: Maybe Text
  , packageAutoImport :: [Text]
  , packageCommonStanzas :: Map Text PackageBuildInfo
  , packageLibraries :: [PackageLibrary]
  , packageExecutables :: [PackageExecutable]
  , packageTests :: [PackageTest]
  , packageIfs :: [Conditional Package]
  }
  deriving (Show)

instance DecodeTOML Package where
  tomlDecoder =
    Package
      <$> getFieldOpt "name"
      <*> getFieldOpt "version"
      <*> (fromMaybe [] <$> getFieldOpt "auto-import")
      <*> getField "common"
      <*> getField "library"
      <*> getField "executable"
      <*> getField "test-suite"
      <*> (fromMaybe [] <$> getFieldOpt "if")

data PackageBuildInfo = PackageBuildInfo
  { packageGhcOptions :: [Text]
  , packageHsSourceDirs :: [Text]
  , packageOtherModules :: [Pattern]
  , packageBuildDepends :: [Text]
  , packageBuildInfoIfs :: [Conditional PackageBuildInfo]
  }
  deriving (Show)

instance DecodeTOML PackageBuildInfo where
  tomlDecoder =
    PackageBuildInfo
      <$> (fromMaybe [] <$> getFieldOpt "ghc-options")
      <*> (fromMaybe [] <$> getFieldOpt "hs-source-dirs")
      <*> (fromMaybe [] <$> getFieldOpt "other-modules")
      <*> (either buildDependsFromTable id . fromMaybe (Right []) <$> getFieldOpt "build-depends")
      <*> (fromMaybe [] <$> getFieldOpt "if")
    where
      buildDependsFromTable = map (\(k, v) -> k <> " " <> v) . Map.toList

data PackageLibrary = PackageLibrary
  { packageLibraryName :: Maybe Text
  , packageExposedModules :: [Pattern]
  , packageLibraryInfo :: PackageBuildInfo
  , packageLibraryIfs :: [Conditional PackageLibrary]
  }
  deriving (Show)

instance DecodeTOML PackageLibrary where
  tomlDecoder =
    PackageLibrary
      <$> getFieldOpt "name"
      <*> (fromMaybe [] <$> getFieldOpt "exposed-modules")
      <*> tomlDecoder
      <*> (fromMaybe [] <$> getFieldOpt "if")

data PackageExecutable = PackageExecutable
  { packageExeImport :: [Text]
  , packageExeName :: Maybe Text
  , packageExeMainIs :: Maybe Text
  , packageExeInfo :: PackageBuildInfo
  , packageExeIfs :: [Conditional PackageExecutable]
  }
  deriving (Show)

instance DecodeTOML PackageExecutable where
  tomlDecoder =
    PackageExecutable
      <$> (fromMaybe [] <$> getFieldOpt "import")
      <*> getFieldOpt "name"
      <*> getFieldOpt "main-is"
      <*> tomlDecoder
      <*> (fromMaybe [] <$> getFieldOpt "if")

data PackageTest = PackageTest Value
  deriving (Show)

instance DecodeTOML PackageTest where
  tomlDecoder = PackageTest <$> tomlDecoder

data Conditional a = Conditional
  { condition :: Text
  , conditionThen :: a
  , conditionElif :: [(Text, a)]
  , conditionElse :: Maybe a
  }
  deriving (Show)

instance DecodeTOML a => DecodeTOML (Conditional a) where
  tomlDecoder = do
    condition <- getField "condition"
    (conditionThen, conditionElif, conditionElse) <- do
      getFieldOpt "then" >>= \case
        Nothing -> (,,) <$> tomlDecoder <*> pure [] <*> pure Nothing
        Just conditionThen -> do
          (,,)
            <$> pure conditionThen
            <*> getFieldWith (getArrayOf decodeElif) "elif"
            <*> getField "else"
    pure Conditional{..}
    where
      decodeElif = (,) <$> getField "condition" <*> tomlDecoder

-- TODO: support globs
type Pattern = Text

parsePackage :: Text -> Either TomcabError Package
parsePackage = first ParseError . decode

resolvePackage :: Package -> Either TomcabError Package
resolvePackage Package{..} = do
  packageExecutables' <- mapM resolvePackageExecutable packageExecutables
  pure
    Package
      { packageExecutables = packageExecutables'
      , ..
      }

resolvePackageExecutable :: PackageExecutable -> Either TomcabError PackageExecutable
resolvePackageExecutable = pure

renderPackage :: Package -> Text
renderPackage = Text.pack . show
