{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Gomoku (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

<<<<<<< HEAD
bindir     = "/home/jm552/.cabal/bin"
libdir     = "/home/jm552/.cabal/lib/x86_64-linux-ghc-8.10.7/Gomoku-0.1.0.0-inplace-gomoku"
dynlibdir  = "/home/jm552/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/jm552/.cabal/share/x86_64-linux-ghc-8.10.7/Gomoku-0.1.0.0"
libexecdir = "/home/jm552/.cabal/libexec/x86_64-linux-ghc-8.10.7/Gomoku-0.1.0.0"
sysconfdir = "/home/jm552/.cabal/etc"
=======
bindir     = "/home/jm551/.cabal/bin"
libdir     = "/home/jm551/.cabal/lib/x86_64-linux-ghc-8.10.7/Gomoku-0.1.0.0-inplace-gomoku"
dynlibdir  = "/home/jm551/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/jm551/.cabal/share/x86_64-linux-ghc-8.10.7/Gomoku-0.1.0.0"
libexecdir = "/home/jm551/.cabal/libexec/x86_64-linux-ghc-8.10.7/Gomoku-0.1.0.0"
sysconfdir = "/home/jm551/.cabal/etc"
>>>>>>> 3ec1ff5a9aaf71df5ebe65daee02e4401cd227b1

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gomoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gomoku_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Gomoku_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Gomoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gomoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gomoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
