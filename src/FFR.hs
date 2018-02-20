module FFR where

import qualified Data.ByteString.Lazy as BL
import Codec.Binary.UTF8.String
import Data.Word
import Data.List
import MatchIt

findFilterRun :: String -> String -> String -> BL.ByteString -> IO ()
findFilterRun find_pattern _ _ = mapM_ (print . findIt ( compile find_pattern) ) . tails . BL.unpack

findIt :: [Match] -> [Word8] -> Maybe [Word8]
findIt m x =
    if isPrefix m x
        then Just x
        else Nothing
