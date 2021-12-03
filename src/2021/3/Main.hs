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

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

getGamma :: [String] -> String
getGamma = intercalate "" . map choosingFunc . group . sort . transpose
    where 
        choosingFunc = \[zeros, ones] -> if length zeros > length ones then "0" else "1"

getEpsilon :: [String] -> String
getEpsilon = intercalate "" . map choosingFunc . group . sort . transpose
    where choosingFunc = \[zeros, ones] -> if length zeros <= length ones then "0" else "1"

findBestMatch :: ([String] -> String) -> [(String, String)] -> Int
findBestMatch _ [(_, solo)] = toDec solo
findBestMatch getBit inp = res
    where
        commonBit = head $ getBit (map fst inp)
        optionsStartingWithCommonBit = filter (\(x:xs, i) -> x == commonBit) inp
        newOpts = map (\(x:xs, original) -> (xs, original)) optionsStartingWithCommonBit
        res = findBestMatch getBit newOpts

part1 :: [String] -> Int
part1 lns = toDec (getGamma lns) * toDec (getEpsilon lns)

part2 :: [String] -> Int
part2 lns = findBestMatch getGamma zippedLns * findBestMatch getEpsilon zippedLns
    where zippedLns = zip lns lns

main :: IO ()
main = do
    contents <- readInputFile
    let lns = lines contents

    print $ part1 lns
    print $ part2 lns
