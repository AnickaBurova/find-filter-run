module MatchIt where

import Data.Word
import Data.List
import qualified Numeric as N

data Match  = Exact Word8
            | Any
            | Error String
            deriving(Show, Eq)

compile :: String -> [Match]
compile [] = []
compile ('X':xs) = Any:compile xs
compile ('x':xs) = Any:compile xs
compile ('d':x:y:z:xs) = Exact ( read [x,y,z] ) : compile xs
compile ('o':x:y:z:xs) = Exact (fst . head $ N.readOct [x,y,z]) : compile xs
compile (x:y:xs) =
    if not ( null parsed)  && ( null . snd $ head parsed)
        then Exact ( fst $ head parsed) : compile xs
        else [Error $ "Cannot parse: " ++ show [x,y] ++ " as hex number"]
    where
        parsed = N.readHex [x,y]
compile x = [Error $ "Cannot parse: " ++ show x]

anyError :: [Match] -> Bool
anyError = any isError

isError :: Match -> Bool
isError (Error _) = True
isError _ = False


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
