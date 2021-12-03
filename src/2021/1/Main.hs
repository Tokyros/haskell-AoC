module Main where
import Data.List
import Data.List.Split
import Text.Regex.Posix
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S

-- Input handling
readInputFile :: IO String
readInputFile = readFile "./input.txt"

readNums :: String -> [Int]
readNums st = map read (lines st)

-- Logic
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

compareAdjacent :: [Int] -> [Ordering]
compareAdjacent = map (\[x, y] -> compare y x) . windows 2

countIncreasing :: [Int] -> Int
countIncreasing = length . filter (==GT) . compareAdjacent

-- Solution
part1 :: [Int] -> Int
part1 = countIncreasing

part2 :: [Int] -> Int
part2 = countIncreasing . map sum . windows 3

main :: IO ()
main = do
    contents <- readInputFile
    let numbers = readNums contents

    print $ part1 numbers
    print $ part2 numbers

