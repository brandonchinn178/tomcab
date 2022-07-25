{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  mergeCommonStanza,

  -- * Shared build information
  PackageBuildInfo (..),
  HasPackageBuildInfo (..),
  emptyPackageBuildInfo,
  decodePackageBuildInfo,

  -- * CabalFields + CabalValue
  CabalFields,
  CabalValue (..),

  -- * Re-exports
  module X,
) where

import Control.Applicative ((<|>))
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
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

import Tomcab.Cabal.Module as X

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

{----- Common stanzas -----}

type CommonStanzas = Map Text CommonStanza

newtype CommonStanza = CommonStanza
  { commonStanzaInfo :: PackageBuildInfo CommonStanza
  }
  deriving (Show, DecodeTOML)

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

{----- Shared build information -----}

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

emptyPackageBuildInfo :: PackageBuildInfo a
emptyPackageBuildInfo = PackageBuildInfo mempty mempty mempty mempty mempty mempty

decodePackageBuildInfo :: DecodeTOML a => Map Text Value -> Decoder (PackageBuildInfo a)
decodePackageBuildInfo = applyDecoder tomlDecoder . Table

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
