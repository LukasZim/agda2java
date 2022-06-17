{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Agda (
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
version = Version [2,6,2,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\cabal\\store\\ghc-9.0.2\\Agda-2.6.2.2-4bb540ba1f3675a789716cb7ca213667cb326a06\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.0.2\\Agda-2.6.2.2-4bb540ba1f3675a789716cb7ca213667cb326a06\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.0.2\\Agda-2.6.2.2-4bb540ba1f3675a789716cb7ca213667cb326a06\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.0.2\\Agda-2.6.2.2-4bb540ba1f3675a789716cb7ca213667cb326a06\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.0.2\\Agda-2.6.2.2-4bb540ba1f3675a789716cb7ca213667cb326a06\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.0.2\\Agda-2.6.2.2-4bb540ba1f3675a789716cb7ca213667cb326a06\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Agda_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Agda_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Agda_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Agda_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Agda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Agda_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
