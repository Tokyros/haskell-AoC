module Main where

import Data.List
import qualified Data.Map as M
import qualified Text.Parsec as P
import Debug.Trace (traceShow)
import Text.Printf (printf)
import qualified Data.Char as C

mapLineToCycleDiff :: String -> [Int]
mapLineToCycleDiff "noop" = [0]
mapLineToCycleDiff st = [0, read (last $ words st)]



runGroupCycle :: [(Int, Int)] -> String
runGroupCycle cycles = res
    where
        something = map (\(idx, value) -> if abs (((idx - 1) `mod` 40) - value) <= 1 then '#' else '.') cycles
        res = something

part1 :: [(Int, Int)] -> Int
part1 values = strengthSum
    where
        cyclesWithSignalStrength = map (\(cycleIndex, cycleValue) -> (cycleIndex, cycleIndex * cycleValue)) values
        strengthSum = sum $ map snd $ filter (\(idx, num) -> (idx - 20) `mod` 40 == 0) cyclesWithSignalStrength

part2 :: [(Int, Int)] -> String
part2 values = code
    where
        groups = groupBy (\(idx, _) (idx2, _) -> ((idx - 1) `quot` 40) == ((idx2 - 1) `quot` 40)) values
        code = unlines $ map runGroupCycle groups

data Command = ADDX Int | NOOP deriving (Read, Show)

main :: IO ()
main = do
    contents <- readFile "./src/2022/10/input.txt"

    let cap = map C.toUpper contents
    print $ (map read (lines cap) :: [Command])

    let commands = lines contents
    let values = zip [1..] $ scanl (+) 1 (concatMap mapLineToCycleDiff commands)

    print $ part1 values
    printf $ part2 values
