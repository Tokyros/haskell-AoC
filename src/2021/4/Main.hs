module Main where
import Data.List
import Data.List.Split
import Text.Regex.Posix
import Control.Applicative
import Data.Char (digitToInt)

import qualified Data.Map as M
import qualified Data.Set as S

readInputFile :: IO String
readInputFile = readFile "./input.txt"

readNums :: String -> [Int]
readNums st = map read (lines st)

part1 :: [String] -> Int
part1 lns = 0

part2 :: [String] -> Int
part2 lns = 0

main :: IO ()
main = do
    contents <- readInputFile
    let lns = lines contents

    print $ part1 lns
    print $ part2 lns
