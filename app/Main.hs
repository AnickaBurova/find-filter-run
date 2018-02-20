{-# LANGUAGE DeriveDataTypeable #-}
module Main where


import qualified Data.ByteString.Lazy as BL
import System.Console.CmdArgs
import Paths_find_filter_run (version)
import Data.Version (showVersion)

import FFR

data Ffr = Ffr { find :: String, filter :: String, run :: String }
    deriving (Show, Data, Typeable)


ffr = Ffr
        { find = def &= argPos 0 &= typ "find-pattern"
        , Main.filter = def &= argPos 1 &= typ "filter-pattern"
        , run = def &= argPos 2 &= typ "script"}
main :: IO ()
main = execute =<< cmdArgs ( ffr
            &= help "Finds some pattern, filter it, run something on data there."
            &= program "ffr"
            &= summary "Find -> filter -> run"
            &= versionArg [summary $ "ffr " ++ showVersion version])


execute :: Ffr -> IO()


execute (Ffr find filter run) = do
    content <- BL.getContents
    findFilterRun find filter run content
