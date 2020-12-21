{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_myfirstapp (
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

bindir     = "/Users/EricFeng/.cabal/bin"
libdir     = "/Users/EricFeng/.cabal/lib/x86_64-osx-ghc-8.8.4/myfirstapp-0.1.0.0-inplace-myfirstapp"
dynlibdir  = "/Users/EricFeng/.cabal/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/EricFeng/.cabal/share/x86_64-osx-ghc-8.8.4/myfirstapp-0.1.0.0"
libexecdir = "/Users/EricFeng/.cabal/libexec/x86_64-osx-ghc-8.8.4/myfirstapp-0.1.0.0"
sysconfdir = "/Users/EricFeng/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "myfirstapp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "myfirstapp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "myfirstapp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "myfirstapp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "myfirstapp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "myfirstapp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
