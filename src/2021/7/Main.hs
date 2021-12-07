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

sumFirstN :: Int -> Int
sumFirstN n = floor (toRational $ (n * (n+1)) `div` 2)

diff :: Int -> Int -> Int
diff a b = abs (a - b)

calculateLowestFuelConsumptions :: (Int -> Int -> Int) -> Inp -> Int
calculateLowestFuelConsumptions cost inp = minimum $ map (\p -> sum $ map (cost p) inp) [minimum inp..maximum inp]

part1 :: Inp -> Int
part1 = calculateLowestFuelConsumptions diff

part2 :: Inp -> Int
part2 = calculateLowestFuelConsumptions (\i p -> sumFirstN (diff i p))

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
