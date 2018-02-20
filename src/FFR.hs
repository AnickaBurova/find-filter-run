module FFR where

import qualified Data.ByteString.Lazy as BL
import Codec.Binary.UTF8.String
import Data.Word
import Data.List
import MatchIt
import Data.Maybe
import System.Process
import System.IO
import Control.Monad

findFilterRun :: String -> String -> String -> BL.ByteString -> IO ()
findFilterRun find_pattern filter_pattern run_script stream = do
        when ( anyError compiled_find ) . error $ "Find pattern error " ++ show compiled_find
        when ( anyError compiled_filter) . error $ "Filter pattern error" ++ show compiled_filter
        mapM_ (run run_script)
            . filter (not.null)
            . map ( filterIt compiled_filter
                    . findIt compiled_find )
            . tails
            $ BL.unpack stream
        where
            compiled_filter = compile filter_pattern
            compiled_find = compile find_pattern


findIt :: [Match] -> [Word8] -> [Word8]
findIt _ [] = []
findIt m all@(x:xs) =
    if isPrefix m all -- find if this substring matches
        then x:takeUntil m xs -- return until the next substring
        else []

filterIt :: [Match] -> [Word8] -> [Word8]
filterIt _ [] = []
filterIt m xs =
    if isPrefix m xs
        then xs
        else []


run :: String -> [Word8] -> IO ()
run fp stream = do
    (Just stdin, _, _ , ph) <- createProcess exec
    hPutStr stdin $ decode stream
    waitForProcess ph
    return ()
    where
        exec = (proc fp []){ std_in = CreatePipe }
