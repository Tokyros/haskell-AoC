module Main where
import Data.List
import Data.List.Split
import Text.Regex.Posix
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S

readInputFile :: IO String
readInputFile = readFile "./input.txt"

readNums :: String -> [Int]
readNums st = map read (lines st)

toTuple :: [String] -> (String, Int)
toTuple [x, y] = (x, read y)
toTuple _ = ("", 0)

parseCommand :: String -> (Int, Int)
parseCommand command =
    case toTuple . words $ command of 
        ("forward", n) -> (n, 0)
        ("down", n) -> (0, n)
        ("up", n) -> (0, -n)
        (_, n) -> (0, -n)


addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x, y) (x', y') = (x + x', y + y')

sumTuples :: [(Int, Int)] -> (Int, Int)
sumTuples = foldl1 addTuple

multiplyTupleComponents :: (Int, Int) -> Int
multiplyTupleComponents (x, y) = x * y

accumulateAims :: Int -> (Int, Int) -> (Int, (Int, Int))
accumulateAims currAim (x, y) = (currAim + y, (x, x * currAim))

calculateWithAims :: [(Int, Int)] -> [(Int, Int)]
calculateWithAims = snd . mapAccumL accumulateAims 0

part1 :: [String] -> Int
part1 = multiplyTupleComponents . sumTuples . map parseCommand

part2 :: [String] -> Int
part2 = multiplyTupleComponents . sumTuples . calculateWithAims . map parseCommand

main :: IO ()
main = do
    contents <- readInputFile

    print $ part1 $ lines contents
    print $ part2 $ lines contents


