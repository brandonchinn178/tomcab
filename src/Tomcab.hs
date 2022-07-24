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
  resolvePackage pkg

data TomcabError
  = ParseError TOMLError
  deriving (Show)

instance Exception TomcabError where
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e

data Package = Package
  { packageName :: Maybe Text
  , packageVersion :: Maybe Text
  , packageCabalVersion :: Maybe Text
  -- build-type
  -- license
  -- license-file
  -- license-files
  -- copyright
  -- author
  -- maintainer
  -- stability
  -- homepage
  -- bug-reports
  -- package-url
  -- synopsis
  -- description
  -- category
  -- tested-with
  -- data-files
  -- data-dir
  -- extra-source-files
  -- extra-doc-files
  -- extra-tmp-files
  , packageCommonStanzas :: Map Text PackageBuildInfo
  , packageLibraries :: [PackageLibrary]
  , packageExecutables :: [PackageExecutable]
  , packageTests :: [PackageTest]
  , packageIfs :: [Conditional Package]
  , packageAutoImport :: [Text]
  }
  deriving (Show)

instance DecodeTOML Package where
  tomlDecoder =
    Package
      <$> getFieldOpt "name"
      <*> getFieldOpt "version"
      <*> getFieldOpt "cabal-version"
      <*> getField "common"
      <*> getField "library"
      <*> getField "executable"
      <*> getField "test-suite"
      <*> (fromMaybe [] <$> getFieldOpt "if")
      <*> (fromMaybe [] <$> getFieldOpt "auto-import")

data PackageBuildInfo = PackageBuildInfo
  { packageBuildDepends :: [Text]
  , packageOtherModules :: [Pattern]
  , packageHsSourceDirs :: [Text]
  -- default-extensions
  -- other-extensions
  , packageDefaultLanguage :: Maybe Text
  -- other-languages
  -- build-tool-depends
  -- buildable
  , packageGhcOptions :: [Text]
  -- ghc-prof-options
  -- ghc-shared-options
  -- ghcjs-options
  -- ghcjs-prof-options
  -- ghcjs-shared-options
  -- includes
  -- install-includes
  -- include-dirs
  -- c-sources
  -- cxx-sources
  -- asm-sources
  -- cmm-sources
  -- js-sources
  -- extra-libraries
  -- extra-ghci-libraries
  -- extra-bundled-libraries
  -- extra-lib-dirs
  -- extra-library-flavours
  -- extra-dynamic-library-flavours
  -- cc-options
  -- cpp-options
  -- cxx-options
  -- cmm-options
  -- asm-options
  -- ld-options
  -- pkgconfig-depends
  -- frameworks
  -- extra-framework-dirs
  -- mixins
  , packageBuildInfoIfs :: [Conditional PackageBuildInfo]
  }
  deriving (Show)

instance DecodeTOML PackageBuildInfo where
  tomlDecoder =
    PackageBuildInfo
      <$> (either buildDependsFromTable id . fromMaybe (Right []) <$> getFieldOpt "build-depends")
      <*> (fromMaybe [] <$> getFieldOpt "other-modules")
      <*> (fromMaybe [] <$> getFieldOpt "hs-source-dirs")
      <*> getFieldOpt "default-language"
      <*> (fromMaybe [] <$> getFieldOpt "ghc-options")
      <*> (fromMaybe [] <$> getFieldOpt "if")
    where
      buildDependsFromTable = map (\(k, v) -> k <> " " <> v) . Map.toList

data PackageLibrary = PackageLibrary
  { packageLibraryName :: Maybe Text
  , packageExposedModules :: [Pattern]
  -- virtual-modules
  -- exposed
  -- visibility
  -- reexported-modules
  -- signatures
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

-- TODO
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

resolvePackage :: Package -> IO Package
resolvePackage Package{..} = do
  -- TODO: manually merge auto-imports into all `imports` sections
  -- TODO: pass all common stanzas to resolve functions
  packageLibraries' <- mapM resolvePackageLibrary packageLibraries
  packageExecutables' <- mapM resolvePackageExecutable packageExecutables
  pure
    Package
      { packageCabalVersion = Just $ fromMaybe "1.12" packageCabalVersion
      , packageLibraries = packageLibraries'
      , packageExecutables = packageExecutables'
      , ..
      }

resolvePackageLibrary :: PackageLibrary -> IO PackageLibrary
resolvePackageLibrary PackageLibrary{..} = do
  -- TODO: inline common stanzas
  -- TODO: load all files in hs-source-dirs
  -- TODO: resolve exposed-modules / other-modules
  pure
    PackageLibrary
      { packageExposedModules = packageExposedModules
      , ..
      }

resolvePackageExecutable :: PackageExecutable -> IO PackageExecutable
resolvePackageExecutable PackageExecutable{..} = do
  -- TODO: inline common stanzas
  -- TODO: load all files in hs-source-dirs
  -- TODO: resolve exposed-modules / other-modules
  pure
    PackageExecutable
      { ..
      }

renderPackage :: Package -> Text
renderPackage = Text.pack . show
