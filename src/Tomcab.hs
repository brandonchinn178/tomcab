{-# LANGUAGE DeriveFunctor #-}
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
import Control.Monad (when, (>=>))
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List (intersperse)
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
import UnliftIO.Exception (Exception (..), fromEither, handleAny, throwIO)

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
  | UnknownCommonStanza Text
  deriving (Show)

instance Exception TomcabError where
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e
    UnknownCommonStanza name -> "Unknown common stanza: " ++ Text.unpack name

data Package = Package
  { packageName :: Maybe Text
  , packageVersion :: Maybe Text
  , packageCabalVersion :: Maybe Text
  , packageCommonStanzas :: CommonStanzas
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

{----- Shared build information -----}

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
  , packageHsSourceDirs :: [Text]
  , packageInfoIfs :: [Conditional a]
  , packageInfoFields :: CabalFields
  }
  deriving (Show, Functor)

instance DecodeTOML a => DecodeTOML (PackageBuildInfo a) where
  tomlDecoder = do
    remainingFields <- getAllExcept ["import", "build-depends", "hs-source-dirs", "if"]
    info <-
      PackageBuildInfo
        <$> getFieldOr "import" []
        <*> (either buildDependsFromTable id . fromMaybe (Right []) <$> getFieldOpt "build-depends")
        <*> getFieldOr "hs-source-dirs" []
        <*> getFieldOr "if" []
        <*> pure Map.empty

    fields <- mapM (applyDecoder tomlDecoder) remainingFields

    pure info{packageInfoFields = fields}
    where
      buildDependsFromTable = map (\(k, v) -> k <> " " <> v) . Map.toList

class HasPackageBuildInfo a where
  modifyBuildInfo :: (PackageBuildInfo a -> PackageBuildInfo a) -> (a -> a)
  modifyBuildInfo f = runIdentity . modifyBuildInfoM (pure . f)

  modifyBuildInfoM :: Monad m => (PackageBuildInfo a -> m (PackageBuildInfo a)) -> (a -> m a)

class FromCommonStanza a where
  fromCommonStanza :: CommonStanza -> a

mergeCommonStanza ::
  (HasPackageBuildInfo a, FromCommonStanza a) =>
  CommonStanza ->
  PackageBuildInfo a ->
  PackageBuildInfo a
mergeCommonStanza (CommonStanza commonInfo) info =
  PackageBuildInfo
    { packageImport = packageImport commonInfo <> packageImport info
    , packageBuildDepends = packageBuildDepends commonInfo <> packageBuildDepends info
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

emptyPackageBuildInfo :: PackageBuildInfo a
emptyPackageBuildInfo = PackageBuildInfo mempty mempty mempty mempty mempty

decodePackageBuildInfo :: DecodeTOML a => Map Text Value -> Decoder (PackageBuildInfo a)
decodePackageBuildInfo = applyDecoder tomlDecoder . Table

type CommonStanzas = Map Text CommonStanza

data CommonStanza = CommonStanza
  { commonStanzaInfo :: PackageBuildInfo CommonStanza
  }
  deriving (Show)

instance DecodeTOML CommonStanza where
  tomlDecoder = CommonStanza <$> tomlDecoder

{----- Components -----}

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

instance HasPackageBuildInfo PackageLibrary where
  modifyBuildInfoM f lib = do
    info <- f (packageLibraryInfo lib)
    pure lib{packageLibraryInfo = info}

instance FromCommonStanza PackageLibrary where
  fromCommonStanza (CommonStanza info) =
    PackageLibrary
      mempty
      mempty
      (fromCommonStanza <$> info)

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

instance HasPackageBuildInfo PackageExecutable where
  modifyBuildInfoM f exe = do
    info <- f (packageExeInfo exe)
    pure exe{packageExeInfo = info}

instance FromCommonStanza PackageExecutable where
  fromCommonStanza (CommonStanza info) =
    PackageExecutable
      mempty
      (fromCommonStanza <$> info)

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
  deriving (Show, Functor)

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
resolvePackage =
  (foldr (>=>) pure)
    [ resolveDefaults
    , resolveAutoImports
    , resolveImports
    , resolveModules
    ]
  where
    resolveDefaults pkg =
      pure
        pkg
          { packageCabalVersion = Just $ fromMaybe "1.12" (packageCabalVersion pkg)
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
      -- TODO: load all files in hs-source-dirs + resolve exposed-modules/other-modules
      packageLibraries' <- mapM pure (packageLibraries pkg)
      packageExecutables' <- mapM pure (packageExecutables pkg)
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
    go (imp:imps) info =
      case imp `Map.lookup` commonStanzas of
        Nothing -> throwIO $ UnknownCommonStanza imp
        Just commonStanza ->
          go
            (packageImport (commonStanzaInfo commonStanza) ++ imps)
            (mergeCommonStanza commonStanza info)

{----- Render -----}

renderPackage :: Package -> Text
renderPackage Package{..} =
  joinLines
    [ renderFieldText "cabal-version" packageCabalVersion
    , "\n"
    , renderFieldText "name" packageName
    , renderFieldText "version" packageVersion
    , renderFields packageFields
    , "\n"
    , stanzas renderLibrary packageLibraries
    , "\n"
    , stanzas renderExecutable packageExecutables
    ]

renderLibrary :: PackageLibrary -> Text
renderLibrary lib =
  joinLines
    [ "library " <> fromMaybe "" (packageLibraryName lib)
    , indent $ renderLibraryBody lib
    ]
  where
    renderLibraryBody PackageLibrary{..} =
      joinLines
        [ renderField "exposed-modules" (CabalListValue packageExposedModules)
        , renderBuildInfo renderLibraryBody packageLibraryInfo
        ]

renderExecutable :: PackageExecutable -> Text
renderExecutable exe =
  joinLines
    [ "executable " <> fromMaybe "" (packageExeName exe)
    , indent $ renderExecutableBody exe
    ]
  where
    renderExecutableBody PackageExecutable{..} =
      joinLines
        [ renderBuildInfo renderExecutableBody packageExeInfo
        ]

renderBuildInfo :: (a -> Text) -> PackageBuildInfo a -> Text
renderBuildInfo renderParent PackageBuildInfo{..} =
  joinLines
    [ renderField "hs-source-dirs" (CabalListValue packageHsSourceDirs)
    , renderField "build-depends" (CabalListValue packageBuildDepends)
    , renderFields packageInfoFields
    , joinLines $
        if null packageInfoIfs
          then []
          else
            [ "\n"
            , stanzas (renderConditional renderParent) packageInfoIfs
            ]
    ]

renderConditional :: (a -> Text) -> Conditional a -> Text
renderConditional renderParent Conditional{..} =
  joinLines $
    [ "if " <> condition
    , indent $ renderParent conditionThen
    , joinLines $
        flip concatMap conditionElif $ \(elifCondition, body) ->
          [ "elif " <> elifCondition
          , indent $ renderParent body
          ]
    , case conditionElse of
        Nothing -> ""
        Just body ->
          joinLines
            [ "else"
            , indent $ renderParent body
            ]
    ]

-- ["a", "b", "", "c", "\n", "d"] => "a\nb\nc\n\nd"
joinLines :: [Text] -> Text
joinLines =
  (Text.intercalate "\n" . map (\s -> if s == "\n" then "" else s)) -- unlines but don't double an explicit "\n"
    . map (Text.dropWhileEnd (== ' ')) -- strip trailing spaces
    . filter (not . Text.null) -- remove empty lines

indent :: Text -> Text
indent = Text.intercalate "\n" . map ("  " <>) . Text.splitOn "\n"

stanzas :: (a -> Text) -> [a] -> Text
stanzas renderStanza = joinLines . intersperse "\n" . map renderStanza

renderFieldText :: Text -> Maybe Text -> Text
renderFieldText label = \case
  Nothing -> ""
  Just v -> renderField label (CabalValue v)

renderField :: Text -> CabalValue -> Text
renderField label = \case
  CabalValue t -> label <> ": " <> t
  CabalListValue [] -> ""
  CabalListValue (t:ts) ->
    joinLines
      [ label <> ":"
      , indent . joinLines $ ("  " <> t) : map (", " <> ) ts
      ]

renderFields :: CabalFields -> Text
renderFields = joinLines . map (uncurry renderField) . Map.toAscList
