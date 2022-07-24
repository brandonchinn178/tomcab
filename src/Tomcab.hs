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

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import TOML (
  Decoder,
  DecodeTOML (..),
  TOMLError,
  Value (Table),
  decode,
  getArrayOf,
  getField,
  getFieldOpt,
  getFieldWith,
  makeDecoder,
  renderTOMLError,
  runDecoder,
 )
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
  , packageCommonStanzas :: Map Text CommonStanza
  , packageLibraries :: [PackageLibrary]
  , packageExecutables :: [PackageExecutable]
  , packageTests :: [PackageTest]
  , packageIfs :: [Conditional Package]
  , packageAutoImport :: [Text]
  , packageFields :: CabalFields
  }
  deriving (Show)

instance DecodeTOML Package where
  tomlDecoder = do
    unused <- getAllExcept
      [ "name"
      , "version"
      , "cabal-version"
      , "common"
      , "library"
      , "executable"
      , "test-suite"
      , "if"
      , "auto-import"
      ]
    package <-
      Package
        <$> getFieldOpt "name"
        <*> getFieldOpt "version"
        <*> getFieldOpt "cabal-version"
        <*> getField "common"
        <*> getField "library"
        <*> getField "executable"
        <*> getField "test-suite"
        <*> getFieldOr "if" []
        <*> getFieldOr "auto-import" []
        <*> pure Map.empty

    fields <- mapM (applyDecoder tomlDecoder) unused

    pure package{packageFields = fields}

type CabalFields = Map Text CabalValue

data CabalValue
   = CabalValue Text
   | CabalListValue [Text]
   deriving (Show)

instance DecodeTOML CabalValue where
  tomlDecoder = (CabalValue <$> tomlDecoder) <|> (CabalListValue <$> tomlDecoder)

data PackageBuildInfo a = PackageBuildInfo
  { packageImport :: [Text]
  , packageBuildDepends :: [Text]
  , packageInfoIfs :: [Conditional a]
  , packageInfoFields :: CabalFields
  }
  deriving (Show)

instance DecodeTOML a => DecodeTOML (PackageBuildInfo a) where
  tomlDecoder = do
    remainingFields <- getAllExcept ["import", "build-depends", "if"]
    info <-
      PackageBuildInfo
        <$> getFieldOr "import" []
        <*> (either buildDependsFromTable id . fromMaybe (Right []) <$> getFieldOpt "build-depends")
        <*> getFieldOr "if" []
        <*> pure Map.empty

    fields <- mapM (applyDecoder tomlDecoder) remainingFields

    pure info{packageInfoFields = fields}
    where
      buildDependsFromTable = map (\(k, v) -> k <> " " <> v) . Map.toList

emptyPackageBuildInfo :: PackageBuildInfo a
emptyPackageBuildInfo = PackageBuildInfo mempty mempty mempty mempty

decodePackageBuildInfo :: DecodeTOML a => Map Text Value -> Decoder (PackageBuildInfo a)
decodePackageBuildInfo = applyDecoder tomlDecoder . Table

data CommonStanza = CommonStanza
  { commonStanzaInfo :: PackageBuildInfo CommonStanza
  }
  deriving (Show)

instance DecodeTOML CommonStanza where
  tomlDecoder = CommonStanza <$> tomlDecoder

data PackageLibrary = PackageLibrary
  { packageLibraryName :: Maybe Text
  , packageExposedModules :: [Pattern]
  , packageLibraryInfo :: PackageBuildInfo PackageLibrary
  }
  deriving (Show)

instance DecodeTOML PackageLibrary where
  tomlDecoder = do
    remainingFields <- getAllExcept ["name", "exposed-modules"]
    library <-
      PackageLibrary
        <$> getFieldOpt "name"
        <*> getFieldOr "exposed-modules" []
        <*> pure emptyPackageBuildInfo

    info <- decodePackageBuildInfo remainingFields

    pure library{packageLibraryInfo = info}

data PackageExecutable = PackageExecutable
  { packageExeName :: Maybe Text
  , packageExeInfo :: PackageBuildInfo PackageExecutable
  }
  deriving (Show)

instance DecodeTOML PackageExecutable where
  tomlDecoder = do
    remainingFields <- getAllExcept ["name"]
    exe <-
      PackageExecutable
        <$> getFieldOpt "name"
        <*> pure emptyPackageBuildInfo

    info <- decodePackageBuildInfo remainingFields

    pure exe{packageExeInfo = info}

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
    remainingFields <- getAllExcept ["condition"]
    condition <- getField "condition"
    (conditionThen, conditionElif, conditionElse) <- do
      getFieldOpt "then" >>= \case
        Nothing ->
          (,,)
            <$> applyDecoder tomlDecoder (Table remainingFields)
            <*> pure []
            <*> pure Nothing
        Just conditionThen -> do
          (,,)
            <$> pure conditionThen
            <*> getFieldWith (getArrayOf decodeElif) "elif"
            <*> getField "else"
    pure Conditional{..}
    where
      decodeElif = (,) <$> getField "condition" <*> tomlDecoder

-- https://github.com/brandonchinn178/toml-reader/issues/12
getAllExcept :: [Text] -> Decoder (Map Text Value)
getAllExcept keys = (`Map.withoutKeys` Set.fromList keys) <$> tomlDecoder

-- https://github.com/brandonchinn178/toml-reader/issues/11
applyDecoder :: Decoder a -> Value -> Decoder a
applyDecoder d v = makeDecoder $ \_ -> runDecoder d v

-- https://github.com/brandonchinn178/toml-reader/issues/10
getFieldOr :: DecodeTOML a => Text -> a -> Decoder a
getFieldOr key def = fromMaybe def <$> getFieldOpt key

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
renderPackage pkg@Package{..} =
  joinLines
    [ field "cabal-version" packageCabalVersion
    , "\n"
    , field "name" packageName
    , field "version" packageVersion
    , "\n"
    , Text.pack $ show pkg
    ]
  where
    -- ["a", "", "b", "\n", "c"] => "a\nb\n\nc"
    joinLines = Text.unlines . map (\s -> if s == "\n" then "" else s) . filter (not . Text.null)

    field label = \case
      Nothing -> ""
      Just v -> label <> ": " <> v
