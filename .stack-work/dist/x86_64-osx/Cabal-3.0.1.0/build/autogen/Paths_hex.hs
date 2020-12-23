{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hex (
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

bindir     = "/Users/EricFeng/Documents/college/columbia_2020-2021/fall/4995_pfp/Parallel_Hex/.stack-work/install/x86_64-osx/e5d801539619ad1d8a1b1f563038ba83cef2b17dada1d5ad2a18b8580236f113/8.8.4/bin"
libdir     = "/Users/EricFeng/Documents/college/columbia_2020-2021/fall/4995_pfp/Parallel_Hex/.stack-work/install/x86_64-osx/e5d801539619ad1d8a1b1f563038ba83cef2b17dada1d5ad2a18b8580236f113/8.8.4/lib/x86_64-osx-ghc-8.8.4/hex-0.1.0.0-BGXKyHZx5kXCt7qtFkgeRY"
dynlibdir  = "/Users/EricFeng/Documents/college/columbia_2020-2021/fall/4995_pfp/Parallel_Hex/.stack-work/install/x86_64-osx/e5d801539619ad1d8a1b1f563038ba83cef2b17dada1d5ad2a18b8580236f113/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/EricFeng/Documents/college/columbia_2020-2021/fall/4995_pfp/Parallel_Hex/.stack-work/install/x86_64-osx/e5d801539619ad1d8a1b1f563038ba83cef2b17dada1d5ad2a18b8580236f113/8.8.4/share/x86_64-osx-ghc-8.8.4/hex-0.1.0.0"
libexecdir = "/Users/EricFeng/Documents/college/columbia_2020-2021/fall/4995_pfp/Parallel_Hex/.stack-work/install/x86_64-osx/e5d801539619ad1d8a1b1f563038ba83cef2b17dada1d5ad2a18b8580236f113/8.8.4/libexec/x86_64-osx-ghc-8.8.4/hex-0.1.0.0"
sysconfdir = "/Users/EricFeng/Documents/college/columbia_2020-2021/fall/4995_pfp/Parallel_Hex/.stack-work/install/x86_64-osx/e5d801539619ad1d8a1b1f563038ba83cef2b17dada1d5ad2a18b8580236f113/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hex_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hex_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hex_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hex_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hex_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hex_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
