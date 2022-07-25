{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Tomcab (
  runTomcab,
) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import TOML (TOMLError, decode, renderTOMLError)
import UnliftIO.Exception (Exception (..), fromEither, handleAny, throwIO)

import Tomcab.Cabal
import Tomcab.Render
import Tomcab.Resolve
import Tomcab.Utils.FilePath

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
        putStrLn $
          "tomcab failed to convert " ++ file ++ ":"
      putStrLn $ displayException e
      exitFailure

findCabalFiles :: IO [FilePath]
findCabalFiles = do
  cwd <- getCurrentDirectory
  filter ((== "package.toml") . takeFileName) <$> listDirectoryRecursive cwd

loadPackage :: FilePath -> IO Package
loadPackage fp = do
  pkg <- parsePackage =<< Text.readFile fp
  -- TODO: loadPackage all files mentioned in `extends`
  resolvePackage pkg

-- TODO: Remove ParseError: https://github.com/brandonchinn178/toml-reader/issues/13
parsePackage :: Text -> IO Package
parsePackage = fromEither . first ParseError . decode

{----- Errors -----}

data TomcabError
  = ParseError TOMLError
  | ResolutionError ResolutionError
  deriving (Show)

instance Exception TomcabError where
  fromException e =
    asum
      [ fromException e -- https://github.com/brandonchinn178/toml-reader/issues/13
      , ResolutionError <$> fromException e
      ]
  displayException = \case
    ParseError e -> Text.unpack $ renderTOMLError e
    ResolutionError e -> displayException e
