{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Tomcab.Render (renderPackage) where

import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Tomcab.Cabal (
  CabalFields,
  CabalValue (..),
  Conditional (..),
  Module,
  Package (..),
  PackageBuildInfo (..),
  PackageExecutable (..),
  PackageLibrary (..),
  pattern Module,
 )

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
        [ renderField "exposed-modules" (CabalListValue $ map renderModule packageExposedModules)
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
    [ renderField "other-modules" (CabalListValue $ map renderModule packageOtherModules)
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
  CabalListValue (t : ts) ->
    joinLines
      [ label <> ":"
      , indent . joinLines $ ("  " <> t) : map (", " <>) ts
      ]

renderFields :: CabalFields -> Text
renderFields = joinLines . map (uncurry renderField) . Map.toAscList
