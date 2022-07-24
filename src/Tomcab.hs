{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tomcab (
  runTomcab,
  generateCabalFile,
) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import TOML (DecodeTOML (..), TOMLError, Value, decode, getField, getFieldOpt, renderTOMLError)
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
        putStrLn $ "tomcab failed to parse " ++ file ++ ":"
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
generateCabalFile fp = fromEither . fmap renderPackage . parsePackage =<< Text.readFile fp

data TomcabError
  = ParseError TOMLError
  deriving (Show)

instance Exception TomcabError where
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e

data Package = Package
  { packageName :: Text
  , packageVersion :: Text
  , packageAutoImport :: [Text]
  , packageCommonStanzas :: Map Text PackageBuildInfo
  , packageLibraries :: [PackageLibrary]
  , packageExecutables :: [PackageExecutable]
  , packageTests :: [PackageTest]
  }
  deriving (Show)

instance DecodeTOML Package where
  tomlDecoder =
    Package
      <$> getField "name"
      <*> getField "version"
      <*> (fromMaybe [] <$> getFieldOpt "auto-import")
      <*> getField "common"
      <*> getField "library"
      <*> getField "executable"
      <*> getField "test-suite"

data PackageBuildInfo = PackageBuildInfo Value
  deriving (Show)

instance DecodeTOML PackageBuildInfo where
  tomlDecoder = PackageBuildInfo <$> tomlDecoder

data PackageLibrary = PackageLibrary Value
  deriving (Show)

instance DecodeTOML PackageLibrary where
  tomlDecoder = PackageLibrary <$> tomlDecoder

data PackageExecutable = PackageExecutable Value
  deriving (Show)

instance DecodeTOML PackageExecutable where
  tomlDecoder = PackageExecutable <$> tomlDecoder

data PackageTest = PackageTest Value
  deriving (Show)

instance DecodeTOML PackageTest where
  tomlDecoder = PackageTest <$> tomlDecoder

parsePackage :: Text -> Either TomcabError Package
parsePackage = first ParseError . decode

renderPackage :: Package -> Text
renderPackage = Text.pack . show
