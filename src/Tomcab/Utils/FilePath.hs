module Tomcab.Utils.FilePath (
  listDirectoryRecursive,
) where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = fmap concat . mapM (go . (dir </>)) =<< listDirectory dir
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]
