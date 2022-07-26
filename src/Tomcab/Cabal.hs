{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tomcab.Cabal (
  Package (..),
  PackageLibrary (..),
  PackageExecutable (..),
  PackageTest (..),
  Conditional (..),

  -- * Common stanzas
  CommonStanzas,
  CommonStanza (..),
  FromCommonStanza (..),

  -- * Shared build information
  PackageBuildInfo (..),
  HasPackageBuildInfo (..),

  -- * CabalFields + CabalValue
  CabalFields,
  CabalValue (..),

  -- * Re-exports
  ModulePattern (..),
  Module,
  pattern Module,
) where

import Control.Applicative ((<|>))
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import TOML (
  DecodeTOML (..),
  Decoder,
  Value (Table),
  getArrayOf,
  getField,
  getFieldOpt,
  getFieldWith,
  makeDecoder,
  runDecoder,
 )

import Tomcab.Cabal.Module
import Tomcab.Resolve.Phases (
  MaybeWhenParsed,
  ResolutionPhase (..),
  Unresolved,
  UnsetFrom,
 )

data Package (phase :: ResolutionPhase) = Package
  { packageName :: MaybeWhenParsed phase Text
  , packageVersion :: Maybe Text
  , packageCabalVersion :: MaybeWhenParsed phase Text
  , packageBuildType :: MaybeWhenParsed phase Text
  , packageCommonStanzas :: UnsetFrom 'NoImports phase (CommonStanzas phase)
  , packageLibraries :: [PackageLibrary phase]
  , packageExecutables :: [PackageExecutable phase]
  , packageTests :: [PackageTest]
  , packageAutoImport :: UnsetFrom 'NoAutoImports phase [Text]
  , packageFields :: CabalFields
  }

instance DecodeTOML (Package Unresolved) where
  tomlDecoder = do
    unused <-
      getAllExcept
        [ "name"
        , "version"
        , "cabal-version"
        , "build-type"
        , "common"
        , "library"
        , "executable"
        , "test-suite"
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
        <*> getFieldOr "auto-import" []
        <*> pure Map.empty

    fields <- mapM (applyDecoder tomlDecoder) unused

    pure package{packageFields = fields}

data PackageLibrary (phase :: ResolutionPhase) = PackageLibrary
  { packageLibraryName :: Maybe Text
  , packageExposedModules :: [ModulePattern]
  , packageLibraryInfo :: PackageBuildInfo phase PackageLibrary
  }

instance DecodeTOML (PackageLibrary Unresolved) where
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
      (mapPackageBuildInfo fromCommonStanza info)

data PackageExecutable (phase :: ResolutionPhase) = PackageExecutable
  { packageExeName :: Maybe Text
  , packageExeInfo :: PackageBuildInfo phase PackageExecutable
  }

instance DecodeTOML (PackageExecutable Unresolved) where
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
      (mapPackageBuildInfo fromCommonStanza info)

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
  deriving (Show, Functor, Foldable, Traversable)

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

{----- Common stanzas -----}

type CommonStanzas (phase :: ResolutionPhase) = Map Text (CommonStanza phase)

newtype CommonStanza (phase :: ResolutionPhase) = CommonStanza
  { commonStanzaInfo :: PackageBuildInfo phase CommonStanza
  }

deriving newtype instance DecodeTOML (CommonStanza Unresolved)

class FromCommonStanza parent where
  fromCommonStanza :: CommonStanza phase -> parent phase

{----- Shared build information -----}

data PackageBuildInfo (phase :: ResolutionPhase) parent = PackageBuildInfo
  { packageImport :: UnsetFrom 'NoImports phase [Text]
  , packageBuildDepends :: [Text]
  , packageOtherModules :: [ModulePattern]
  , packageHsSourceDirs :: [Text]
  , packageInfoIfs :: [Conditional (parent phase)]
  , packageInfoFields :: CabalFields
  }

instance DecodeTOML (parent Unresolved) => DecodeTOML (PackageBuildInfo Unresolved parent) where
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

class HasPackageBuildInfo parent where
  getBuildInfo :: parent phase -> PackageBuildInfo phase parent

  modifyBuildInfo :: (PackageBuildInfo phase1 parent -> PackageBuildInfo phase2 parent) -> (parent phase1 -> parent phase2)
  modifyBuildInfo f = runIdentity . modifyBuildInfoM (pure . f)

  modifyBuildInfoM :: Monad m => (PackageBuildInfo phase1 parent -> m (PackageBuildInfo phase2 parent)) -> (parent phase1 -> m (parent phase2))

emptyPackageBuildInfo :: PackageBuildInfo Unresolved parent
emptyPackageBuildInfo = PackageBuildInfo mempty mempty mempty mempty mempty mempty

decodePackageBuildInfo :: DecodeTOML (parent Unresolved) => Map Text Value -> Decoder (PackageBuildInfo Unresolved parent)
decodePackageBuildInfo = applyDecoder tomlDecoder . Table

mapPackageBuildInfo :: (parent1 phase -> parent2 phase) -> PackageBuildInfo phase parent1 -> PackageBuildInfo phase parent2
mapPackageBuildInfo f info =
  info
    { packageInfoIfs = map (fmap f) (packageInfoIfs info)
    }

{----- CabalFields + CabalValue -----}

type CabalFields = Map Text CabalValue

data CabalValue
  = CabalValue Text
  | CabalListValue [Text]
  deriving (Show)

instance DecodeTOML CabalValue where
  tomlDecoder = (CabalValue <$> tomlDecoder) <|> (CabalListValue <$> tomlDecoder)

{----- Decoder utilities -----}

-- https://github.com/brandonchinn178/toml-reader/issues/10
getFieldOr :: DecodeTOML a => Text -> a -> Decoder a
getFieldOr key def = fromMaybe def <$> getFieldOpt key

-- https://github.com/brandonchinn178/toml-reader/issues/11
applyDecoder :: Decoder a -> Value -> Decoder a
applyDecoder d v = makeDecoder $ \_ -> runDecoder d v

-- https://github.com/brandonchinn178/toml-reader/issues/12
getAllExcept :: [Text] -> Decoder (Map Text Value)
getAllExcept keys = (`Map.withoutKeys` Set.fromList keys) <$> tomlDecoder
