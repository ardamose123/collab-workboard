module Paths_workboard_server (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/ambervale/Projects/Actuales/workboard/workboard-server/.stack-work/install/x86_64-linux/lts-6.0/7.10.3/bin"
libdir     = "/ambervale/Projects/Actuales/workboard/workboard-server/.stack-work/install/x86_64-linux/lts-6.0/7.10.3/lib/x86_64-linux-ghc-7.10.3/workboard-server-0.1.0.0-8elFWjCEJiy5Rr29VHtdL4"
datadir    = "/ambervale/Projects/Actuales/workboard/workboard-server/.stack-work/install/x86_64-linux/lts-6.0/7.10.3/share/x86_64-linux-ghc-7.10.3/workboard-server-0.1.0.0"
libexecdir = "/ambervale/Projects/Actuales/workboard/workboard-server/.stack-work/install/x86_64-linux/lts-6.0/7.10.3/libexec"
sysconfdir = "/ambervale/Projects/Actuales/workboard/workboard-server/.stack-work/install/x86_64-linux/lts-6.0/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "workboard_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "workboard_server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "workboard_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "workboard_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "workboard_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
