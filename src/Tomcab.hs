{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Tomcab (
  runTomcab,
) where

import Control.Monad (when)
import Data.Foldable (asum)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import TOML (TOMLError, decode)
import UnliftIO.Exception (Exception (..), fromEither, handleAny)

import Tomcab.Cabal
import Tomcab.Render
import Tomcab.Resolve
import Tomcab.Resolve.Phases
import Tomcab.Utils.FilePath
import Tomcab.Utils.TOML ()

runTomcab :: Maybe [FilePath] -> IO ()
runTomcab = \case
  Just files -> go files
  Nothing -> findCabalFiles >>= go
  where
    go = mapM_ $ \file ->
      handleAny (onError file) $ do
        pkg <- loadPackage file
        let cabalPath = takeDirectory file </> (Text.unpack (packageName pkg) ++ ".cabal")
        Text.writeFile cabalPath (renderPackage pkg)

    onError file e = do
      when (isJust $ fromException @TomcabError e) $
        putStrLn $
          "tomcab failed to convert " ++ file ++ ":"
      putStrLn $ displayException e
      exitFailure

findCabalFiles :: IO [FilePath]
findCabalFiles = do
  cwd <- getCurrentDirectory
  filter ((== "package.toml") . takeFileName) <$> listDirectoryRecursive cwd

loadPackage :: FilePath -> IO (Package Resolved)
loadPackage fp = do
  pkg <- parsePackage =<< Text.readFile fp
  -- TODO: loadPackage all files mentioned in `extends`
  resolvePackage pkg

parsePackage :: Text -> IO (Package Unresolved)
parsePackage = fromEither . decode

{----- Errors -----}

data TomcabError
  = ParseError TOMLError
  | ResolutionError ResolutionError
  deriving (Show)

instance Exception TomcabError where
  fromException e =
    asum
      [ ParseError <$> fromException e
      , ResolutionError <$> fromException e
      ]
  displayException = \case
    ParseError e -> displayException e
    ResolutionError e -> displayException e
