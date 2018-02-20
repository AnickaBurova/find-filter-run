module MatchIt where

import Data.Word
import qualified Numeric as N

data Match  = Exact Word8
            | Any
            | Error
            deriving(Show)

compile :: String -> [Match]
compile [] = []
compile ('X':xs) = Any:compile xs
compile ('x':xs) = Any:compile xs
compile (x:y:xs) = Exact (fst . head $ N.readHex [x,y]) : compile xs
compile _ = [Error]


isPrefix :: [Match] -> [Word8] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (Any:xs) (_:ys) = isPrefix xs ys
isPrefix (Exact x:xs) (y:ys)
    | x == y = isPrefix xs ys
    | otherwise = False


dropUntil :: [Match] -> [Word8] -> [Word8]
dropUntil [] xs = xs
dropUntil _ [] = []
dropUntil m all@(x:xs) =
    if isPrefix m all
        then all
        else dropUntil m xs

takeUntil :: [Match] -> [Word8] -> [Word8]
takeUntil [] xs = xs
takeUntil _ [] = []
takeUntil m all@(x:xs) =
    if isPrefix m all
        then []
        else x : takeUntil m xs

{-isMatch :: [Match] -> [Word8] -> Bool-}
