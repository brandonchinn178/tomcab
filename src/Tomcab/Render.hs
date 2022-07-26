{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tomcab.Render (renderPackage) where

import Data.List (intersperse)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Tomcab.Cabal (
  CabalFields,
  CabalValue (..),
  Conditional (..),
  Module (..),
  Package (..),
  PackageBuildInfo (..),
  PackageExecutable (..),
  PackageLibrary (..),
  PackageTestSuite (..),
 )
import Tomcab.Resolve.Phases (Resolved)

renderPackage :: Package Resolved -> Text
renderPackage Package{..} =
  stripTrailingSpaces . joinLines $
    [ field "cabal-version" packageCabalVersion
    , "\n"
    , field "name" packageName
    , field "version" packageVersion
    , field "build-type" packageBuildType
    , fields packageFields
    , "\n"
    , stanzas renderLibrary packageLibraries
    , "\n"
    , stanzas renderExecutable packageExecutables
    , "\n"
    , stanzas renderTestSuite packageTestSuites
    ]
  where
    stripTrailingSpaces = Text.unlines . map Text.stripEnd . Text.lines

renderLibrary :: PackageLibrary Resolved -> Text
renderLibrary lib =
  joinLines
    [ "library " <> fromMaybe "" (packageLibraryName lib)
    , indent $ renderLibraryBody lib
    ]
  where
    renderLibraryBody PackageLibrary{..} =
      joinLines
        [ field "exposed-modules" (map renderModule packageExposedModules)
        , renderBuildInfo renderLibraryBody packageLibraryInfo
        ]

renderExecutable :: PackageExecutable Resolved -> Text
renderExecutable exe =
  joinLines
    [ "executable " <> fromMaybe "" (packageExeName exe)
    , indent $ renderExecutableBody exe
    ]
  where
    renderExecutableBody PackageExecutable{..} =
      joinLines
        [ field "main-is" packageExeMainIs
        , renderBuildInfo renderExecutableBody packageExeInfo
        ]

renderTestSuite :: PackageTestSuite Resolved -> Text
renderTestSuite test =
  joinLines
    [ "test-suite " <> fromMaybe "" (packageTestName test)
    , indent $ renderTestBody test
    ]
  where
    renderTestBody PackageTestSuite{..} =
      joinLines
        [ field "type" packageTestType
        , field "main-is" packageTestMainIs
        , renderBuildInfo renderTestBody packageTestInfo
        ]

renderBuildInfo :: (parent Resolved -> Text) -> PackageBuildInfo Resolved parent -> Text
renderBuildInfo renderParent PackageBuildInfo{..} =
  joinLines
    [ field "other-modules" (map renderModule packageOtherModules)
    , field "hs-source-dirs" packageHsSourceDirs
    , field "build-depends" packageBuildDepends
    , fields packageInfoFields
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
  joinLines
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

renderModule :: Module -> Text
renderModule (Module path) = Text.intercalate "." path

{----- Helpers -----}

-- ["a", "b", "", "c", "\n", "d"] => "a\nb\nc\n\nd"
joinLines :: [Text] -> Text
joinLines =
  (Text.intercalate "\n" . map (\s -> if s == "\n" then "" else s)) -- unlines but don't double an explicit "\n"
    . filter (not . Text.null) -- remove empty lines

indent :: Text -> Text
indent = Text.intercalate "\n" . map ("  " <>) . Text.splitOn "\n"

stanzas :: (a -> Text) -> [a] -> Text
stanzas renderStanza = joinLines . intersperse "\n" . map renderStanza

class ToCabalField a where
  toCabalFieldMaybe :: a -> Maybe CabalValue

instance ToCabalField CabalValue where
  toCabalFieldMaybe = Just
instance ToCabalField Text where
  toCabalFieldMaybe = Just . CabalValue
instance ToCabalField [Text] where
  toCabalFieldMaybe = Just . CabalListValue
instance ToCabalField a => ToCabalField (Maybe a) where
  toCabalFieldMaybe = (>>= toCabalFieldMaybe)

field :: ToCabalField a => Text -> a -> Text
field label a =
  case toCabalFieldMaybe a of
    Nothing -> ""
    Just (CabalValue v) -> label <> ": " <> v
    Just (CabalListValue vals) ->
      case map renderListVal vals of
        [] -> ""
        v : vs ->
          joinLines
            [ label <> ":"
            , indent . joinLines $ ("  " <> v) : map (Text.pack [sep, ' '] <>) vs
            ]
  where
    (sep, renderListVal)
      | label `Set.member` tokenListFields = (' ', quote)
      | otherwise = (',', id)
    quote s = "\"" <> s <> "\""

fields :: CabalFields -> Text
fields = joinLines . map (uncurry field) . Map.toAscList

-- | Cabal fields typed as 'token list', which should NOT use commas.
tokenListFields :: Set Text
tokenListFields =
  Set.fromList
    [ "ghc-options"
    , "ghc-prof-options"
    , "ghc-shared-options"
    , "ghcjs-options"
    , "ghcjs-prof-options"
    , "ghcjs-shared-options"
    , "extra-libraries"
    , "extra-ghci-libraries"
    , "extra-bundled-libraries"
    , "cc-options"
    , "cpp-options"
    , "cxx-options"
    , "cmm-options"
    , "asm-options"
    , "ld-options"
    , "frameworks"
    ]
