module Main where
import Data.List
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = inp
    where
        inp = map read (splitOn  "," str)

type Inp = [Int]

part1 :: Inp -> Int
part1 inp = res
    where 
        fuels = map (\p -> sum $ map (\i -> abs (i - p)) inp) inp
        min = minimum fuels
        res = min

part2 :: Inp -> Int
part2 inp = res
    where 
        minInp = minimum inp
        maxInp = maximum inp
        adjInp = [minInp..maxInp]
        fuels = map (\p -> sum $ map (\i -> sum [1..(abs (i-p))]) inp) adjInp
        min = minimum fuels
        res = min

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
