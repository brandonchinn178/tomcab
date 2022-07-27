{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tomcab.Cabal (
  Package (..),
  PackageLibrary (..),
  PackageExecutable (..),
  PackageTestSuite (..),
  Conditional (..),
  ImportField,
  TestTypeField,

  -- * Common stanzas
  CommonStanzas,
  CommonStanza (..),

  -- * Shared build information
  PackageBuildInfo (..),

  -- * CabalFields + CabalValue
  CabalFields,
  CabalValue (..),

  -- * Re-exports
  module Tomcab.Cabal.Module,
) where

import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import TOML (
  DecodeTOML (..),
  Decoder,
  Value (Table),
  getArrayOf,
  getField,
  getFieldOpt,
  getFieldWith,
 )

import Tomcab.Cabal.Module
import Tomcab.Resolve.Phases (
  NonNullAfterParsed,
  ResolutionPhase (..),
  Unresolved,
  UnsetFrom,
 )
import Tomcab.Utils.TOML

data Package (phase :: ResolutionPhase) = Package
  { packageName :: NonNullAfterParsed phase Text
  , packageVersion :: Maybe Text
  , packageCabalVersion :: NonNullAfterParsed phase Text
  , packageBuildType :: NonNullAfterParsed phase Text
  , packageCommonStanzas :: UnsetFrom 'NoImports phase (CommonStanzas phase)
  , packageLibraries :: [PackageLibrary phase]
  , packageExecutables :: [PackageExecutable phase]
  , packageTestSuites :: [PackageTestSuite phase]
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
  , packageExposedModules :: [ModulePath phase]
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

data PackageExecutable (phase :: ResolutionPhase) = PackageExecutable
  { packageExeName :: Maybe Text
  , packageExeMainIs :: Maybe Text
  , packageExeInfo :: PackageBuildInfo phase PackageExecutable
  }

instance DecodeTOML (PackageExecutable Unresolved) where
  tomlDecoder = do
    remainingFields <- getAllExcept ["name", "main-is"]
    exe <-
      PackageExecutable
        <$> getFieldOpt "name"
        <*> getFieldOpt "main-is"
        <*> pure emptyPackageBuildInfo

    info <- decodePackageBuildInfo remainingFields

    pure exe{packageExeInfo = info}

type TestTypeField (phase :: ResolutionPhase) = NonNullAfterParsed phase Text

data PackageTestSuite (phase :: ResolutionPhase) = PackageTestSuite
  { packageTestName :: Maybe Text
  , packageTestType :: TestTypeField phase
  , packageTestMainIs :: Maybe Text
  , packageTestInfo :: PackageBuildInfo phase PackageTestSuite
  }

instance DecodeTOML (PackageTestSuite Unresolved) where
  tomlDecoder = do
    remainingFields <- getAllExcept ["name", "type", "main-is"]
    test <-
      PackageTestSuite
        <$> getFieldOpt "name"
        <*> getFieldOpt "type"
        <*> getFieldOpt "main-is"
        <*> pure emptyPackageBuildInfo

    info <- decodePackageBuildInfo remainingFields

    pure test{packageTestInfo = info}

data Conditional a = Conditional
  { condition :: Text
  , conditionThen :: a
  , conditionElif :: [(Text, a)]
  , conditionElse :: Maybe a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

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

{----- Shared build information -----}

type ImportField (phase :: ResolutionPhase) = UnsetFrom 'NoImports phase [Text]

data PackageBuildInfo (phase :: ResolutionPhase) parent = PackageBuildInfo
  { packageImport :: ImportField phase
  , packageBuildDepends :: [Text]
  , packageOtherModules :: [ModulePath phase]
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

emptyPackageBuildInfo :: PackageBuildInfo Unresolved parent
emptyPackageBuildInfo = PackageBuildInfo mempty mempty mempty mempty mempty mempty

decodePackageBuildInfo :: DecodeTOML (parent Unresolved) => Map Text Value -> Decoder (PackageBuildInfo Unresolved parent)
decodePackageBuildInfo = applyDecoder tomlDecoder . Table

{----- CabalFields + CabalValue -----}

type CabalFields = Map Text CabalValue

data CabalValue
  = CabalValue Text
  | CabalListValue [Text]
  deriving (Show)

instance DecodeTOML CabalValue where
  tomlDecoder = (CabalValue <$> tomlDecoder) <|> (CabalListValue <$> tomlDecoder)
