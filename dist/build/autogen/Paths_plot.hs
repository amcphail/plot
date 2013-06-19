module Paths_plot (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,5], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/vivian/.cabal/bin"
libdir     = "/home/vivian/.cabal/lib/plot-0.1.5/ghc-7.4.2"
datadir    = "/home/vivian/.cabal/share/plot-0.1.5"
libexecdir = "/home/vivian/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "plot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "plot_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "plot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "plot_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
