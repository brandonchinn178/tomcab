{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tomcab (
  runTomcab,
) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when, (>=>))
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List (find, intersperse, isPrefixOf, sort, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, takeFileName, (</>))
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
      handleAny (onError file) $ do
        pkg <- loadPackage file
        cabalPath <-
          case packageName pkg of
            Nothing -> throwIO MissingPackageName
            Just name -> pure $ takeDirectory file </> (Text.unpack name ++ ".cabal")
        Text.writeFile cabalPath (renderPackage pkg)

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

{----- TODO: move to another module -----}

loadPackage :: FilePath -> IO Package
loadPackage fp = do
  pkg <- fromEither . parsePackage =<< Text.readFile fp
  -- TODO: loadPackage all files mentioned in `extends`
  resolvePackage pkg

data TomcabError
  = ParseError TOMLError
  | InvalidPattern ModulePattern
  | MissingPackageName
  | UnknownCommonStanza Text
  deriving (Show)

instance Exception TomcabError where
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e
    InvalidPattern pat -> "Invalid pattern: " ++ Text.unpack (renderPattern pat)
    MissingPackageName -> "Package name is not specified"
    UnknownCommonStanza name -> "Unknown common stanza: " ++ Text.unpack name

-- invariant: after resolveDefaults, packageCabalVersion is Just
-- invariant: after resolveDefaults, packageBuildType is Just
-- invariant: after resolveAutoImports, packageAutoImport is empty
-- invariant: after resolveImports, packageCommonStanzas is empty
data Package = Package
  { packageName :: Maybe Text
  , packageVersion :: Maybe Text
  , packageCabalVersion :: Maybe Text
  , packageBuildType :: Maybe Text
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
      , "build-type"
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
        <*> getFieldOpt "build-type"
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

-- invariant: after resolveImports, packageImport is empty
data PackageBuildInfo a = PackageBuildInfo
  { packageImport :: [Text]
  , packageBuildDepends :: [Text]
  , packageOtherModules :: [ModulePattern]
  , packageHsSourceDirs :: [Text]
  , packageInfoIfs :: [Conditional a]
  , packageInfoFields :: CabalFields
  }
  deriving (Show, Functor)

instance DecodeTOML a => DecodeTOML (PackageBuildInfo a) where
  tomlDecoder = do
    remainingFields <- getAllExcept ["import", "build-depends", "other-modules", "hs-source-dirs", "if"]
    info <-
      PackageBuildInfo
        <$> getFieldOr "import" []
        <*> (either buildDependsFromTable id . fromMaybe (Right []) <$> getFieldOpt "build-depends")
        <*> getFieldOr "other-modules" []
        <*> getFieldOr "hs-source-dirs" []
        <*> getFieldOr "if" []
        <*> pure Map.empty

    fields <- mapM (applyDecoder tomlDecoder) remainingFields

    pure info{packageInfoFields = fields}
    where
      buildDependsFromTable = map (\(k, v) -> k <> " " <> v) . Map.toList

class HasPackageBuildInfo a where
  getBuildInfo :: a -> PackageBuildInfo a

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

emptyPackageBuildInfo :: PackageBuildInfo a
emptyPackageBuildInfo = PackageBuildInfo mempty mempty mempty mempty mempty mempty

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
  , packageExposedModules :: [ModulePattern]
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
  getBuildInfo = packageLibraryInfo
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
  getBuildInfo = packageExeInfo
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

parsePackage :: Text -> Either TomcabError Package
parsePackage = first ParseError . decode

-- TODO: add "phase" of resolution in phantom type? a la Trees That Grow
resolvePackage :: Package -> IO Package
resolvePackage =
  (foldr (>=>) pure)
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
    go (imp:imps) info =
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

  let parseAndTagPattern tag pat = (, tag) <$> fromEither (parsePattern pat)
  patterns <-
    fmap concat . sequence $
      [ mapM (parseAndTagPattern Exposed) exposedModules
      , mapM (parseAndTagPattern Other) packageOtherModules
      ]

  let matchedModules = zipMapMaybe (categorize patterns) modules
      extract x = map fst . filter ((== x) . snd)
  pure (extract Exposed matchedModules, extract Other matchedModules)
  where
    PackageBuildInfo{packageHsSourceDirs, packageOtherModules} = getBuildInfo parent

    concatMapM f = fmap concat . mapM f
    zipMapMaybe f = mapMaybe (\x -> (x,) <$> f x)

{----- Patterns -----}

data ParsedModulePattern = ParsedModulePattern
  { patternPath :: [Text]
  , patternHasWildcard :: Bool
  -- ^ Does the pattern end with a wildcard?
  }
  deriving (Show, Eq, Ord)

-- TODO: make a separate type
type ModulePattern = ParsedModulePattern
pattern ModulePattern :: Text -> ParsedModulePattern
pattern ModulePattern path = ParsedModulePattern [path] False
{-# COMPLETE ModulePattern #-}

instance DecodeTOML ModulePattern where
  tomlDecoder = ModulePattern <$> tomlDecoder

-- TODO: make a separate type
-- invariant: patternHasWildcard is False
type Module = ModulePattern
pattern Module :: [Text] -> ParsedModulePattern
pattern Module path = ParsedModulePattern path False
{-# COMPLETE Module #-}

parsePattern :: ModulePattern -> Either TomcabError ParsedModulePattern
parsePattern pat@(ModulePattern path) =
  case NonEmpty.nonEmpty $ Text.splitOn "." path of
    Nothing -> Left $ InvalidPattern pat
    Just path' -> do
      let (patternPath, patternHasWildcard) =
            case unsnoc path' of
              (prefix, "*") -> (prefix, True)
              _ -> (NonEmpty.toList path', False)
      forM_ patternPath $ \case
        "*" -> Left $ InvalidPattern pat
        _ -> pure ()
      pure ParsedModulePattern{..}
  where
    unsnoc xs = (NonEmpty.init xs, NonEmpty.last xs)

renderPattern :: ParsedModulePattern -> Text
renderPattern ParsedModulePattern{..} =
  Text.concat
    [ Text.intercalate "." patternPath
    , if patternHasWildcard then ".*" else ""
    ]

-- | Return the annotation of the closest matching pattern for the given module.
categorize :: [(ParsedModulePattern, a)] -> Module -> Maybe a
categorize patterns (Module modl) =
  fmap snd . find isMatch . sortByPathDesc . filter isPossibleMatch $ patterns
  where
    sortByPathDesc = sortOn (Down . length . patternPath . fst)
    isPossibleMatch = (`isPrefixOf` modl) . patternPath . fst
    isMatch (ParsedModulePattern{..}, _) = patternHasWildcard || patternPath == modl

{----- Render -----}

renderPackage :: Package -> Text
renderPackage Package{..} =
  stripTrailingSpaces . joinLines $
    [ renderFieldText "cabal-version" packageCabalVersion
    , "\n"
    , renderFieldText "name" packageName
    , renderFieldText "version" packageVersion
    , renderFieldText "build-type" packageBuildType
    , renderFields packageFields
    , "\n"
    , stanzas renderLibrary packageLibraries
    , "\n"
    , stanzas renderExecutable packageExecutables
    ]
  where
    stripTrailingSpaces = Text.unlines . map Text.stripEnd . Text.lines

renderLibrary :: PackageLibrary -> Text
renderLibrary lib =
  joinLines
    [ "library " <> fromMaybe "" (packageLibraryName lib)
    , indent $ renderLibraryBody lib
    ]
  where
    renderLibraryBody PackageLibrary{..} =
      joinLines
        [ renderField "exposed-modules" (CabalListValue $ map renderPattern packageExposedModules)
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
    [ renderField "other-modules" (CabalListValue $ map renderPattern packageOtherModules)
    , renderField "hs-source-dirs" (CabalListValue packageHsSourceDirs)
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
