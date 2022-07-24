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
import Data.Functor.Identity (Identity)
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
  | PackageMissing Text
  | ExecutableMissing (Maybe Text) Text
  deriving (Show)

instance Exception TomcabError where
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e
    PackageMissing field -> "Package missing field: " ++ Text.unpack field
    ExecutableMissing name field ->
      concat
        [ "Executable"
        , case name of
            Nothing -> ""
            Just name' -> " '" ++ Text.unpack name' ++ "'"
        , " missing field: "
        , Text.unpack field
        ]

data PackageF f = Package
  { packageName :: f Text
  , packageVersion :: f Text
  , packageAutoImport :: [Text]
  , packageCommonStanzas :: Map Text PackageBuildInfo
  , packageLibraries :: [PackageLibrary]
  , packageExecutables :: [PackageExecutableF f]
  , packageTests :: [PackageTest]
  , packageIfs :: [Conditional RawPackage]
  }

type RawPackage = PackageF Maybe
deriving instance Show RawPackage

type Package = PackageF Identity
deriving instance Show Package

instance DecodeTOML RawPackage where
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

data PackageExecutableF f = PackageExecutable
  { packageExeImport :: [Text]
  , packageExeName :: f Text
  , packageExeMainIs :: f Text
  , packageExeInfo :: PackageBuildInfo
  , packageExeIfs :: [Conditional RawPackageExecutable]
  }

type RawPackageExecutable = PackageExecutableF Maybe
deriving instance Show RawPackageExecutable

type PackageExecutable = PackageExecutableF Identity
deriving instance Show PackageExecutable

instance DecodeTOML RawPackageExecutable where
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

parsePackage :: Text -> Either TomcabError RawPackage
parsePackage = first ParseError . decode

resolvePackage :: RawPackage -> Either TomcabError Package
resolvePackage Package{..} = do
  packageName' <- maybe (Left $ PackageMissing "name") pure packageName
  packageVersion' <- maybe (Left $ PackageMissing "version") pure packageVersion
  packageExecutables' <- mapM resolvePackageExecutable packageExecutables
  pure
    Package
      { packageName = pure packageName'
      , packageVersion = pure packageVersion'
      , packageExecutables = packageExecutables'
      , ..
      }

resolvePackageExecutable :: RawPackageExecutable -> Either TomcabError PackageExecutable
resolvePackageExecutable PackageExecutable{..} = do
  packageExeName' <- maybe (Left $ ExecutableMissing Nothing "name") pure packageExeName
  packageExeMainIs' <- maybe (Left $ ExecutableMissing (Just packageExeName') "main-is") pure packageExeMainIs
  pure
    PackageExecutable
      { packageExeName = pure packageExeName'
      , packageExeMainIs = pure packageExeMainIs'
      , ..
      }

renderPackage :: Package -> Text
renderPackage = Text.pack . show
