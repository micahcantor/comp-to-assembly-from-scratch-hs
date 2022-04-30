{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_comp_to_assembly_from_scratch_hs (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/hydroxide/.cabal/bin"
libdir     = "/home/hydroxide/.cabal/lib/x86_64-linux-ghc-8.10.7/comp-to-assembly-from-scratch-hs-0.1.0.0-inplace"
dynlibdir  = "/home/hydroxide/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/hydroxide/.cabal/share/x86_64-linux-ghc-8.10.7/comp-to-assembly-from-scratch-hs-0.1.0.0"
libexecdir = "/home/hydroxide/.cabal/libexec/x86_64-linux-ghc-8.10.7/comp-to-assembly-from-scratch-hs-0.1.0.0"
sysconfdir = "/home/hydroxide/.cabal/etc"

getBinDir     = catchIO (getEnv "comp_to_assembly_from_scratch_hs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "comp_to_assembly_from_scratch_hs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "comp_to_assembly_from_scratch_hs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "comp_to_assembly_from_scratch_hs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "comp_to_assembly_from_scratch_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "comp_to_assembly_from_scratch_hs_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
