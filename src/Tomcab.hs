{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tomcab (
  generateCabalFiles,
  generateCabalFile,
) where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeFileName, (</>))
import TOML (DecodeTOML (..), TOMLError, decode)
import UnliftIO.Exception (Exception, fromEither)

generateCabalFiles :: Maybe [FilePath] -> IO ()
generateCabalFiles = \case
  Just files -> go files
  Nothing -> findCabalFiles >>= go
  where
    go = mapM_ $ \file ->
      -- TODO
      generateCabalFile file >>= Text.putStrLn

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
generateCabalFile fp = fromEither . fmap renderPackage . parsePackage =<< Text.readFile fp

data ParseError
  = TOMLError TOMLError
  deriving (Show)

instance Exception ParseError

data Package = Package
  deriving (Show)

instance DecodeTOML Package where
  tomlDecoder = pure Package

parsePackage :: Text -> Either ParseError Package
parsePackage = first TOMLError . decode

renderPackage :: Package -> Text
renderPackage = Text.pack . show
