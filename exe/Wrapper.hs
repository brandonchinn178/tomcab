{-# LANGUAGE CPP #-}

module Wrapper (wrapCommand) where

import System.Environment (getArgs)
import Tomcab (runTomcab)

#ifndef mingw32_HOST_OS
import System.Posix.Process (executeFile)
#else
import System.Process (callProcess)
#endif

wrapCommand :: String -> IO ()
wrapCommand cmd = do
  runTomcab Nothing
  execFile cmd =<< getArgs

execFile :: FilePath -> [String] -> IO ()
#ifndef mingw32_HOST_OS
execFile cmd args = executeFile cmd True args Nothing
#else
execFile = callProcess
#endif
