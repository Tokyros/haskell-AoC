module Main where

import Data.List ( find, group, sort, tails, findIndex, transpose )
import Data.Maybe (fromJust)
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Text.Parsec (Parsec, runParser, anyChar)

safeFindIndex :: (a -> Bool) -> [a] -> Int
safeFindIndex fn list = case findIndex fn list of
                            Just n -> n
                            Nothing -> -1

windows :: Int -> [a] -> [[a]]
windows m = transpose . take m . tails

isUniqueString :: String -> Bool
isUniqueString st = length (group $ sort st) == length st

getFirstRepeating :: Int -> String -> Int
getFirstRepeating len st = len + safeFindIndex isUniqueString (windows len st)


part1 = getFirstRepeating 4
part2 = getFirstRepeating 14

main :: IO ()
main = do
    contents <- readFile "./src/2022/6/input.txt"    

    print $ part1 contents
    print $ part2 contents

