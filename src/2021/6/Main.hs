module Main where
import Data.List
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = map read inp
    where
        inp = splitOn "," str


type Inp = [Int]

passDay :: Inp -> Inp
passDay fish = do
        f <- fish
        if f == 0 then [8, 6] else [f-1]

passDays :: Int -> Inp -> Inp
passDays 1 f = passDay f
passDays n f = passDays (n-1) (passDay f)

part1 :: Inp -> Int
part1 inp = length res
    where 
        res = passDays 80 inp

step :: [Int] -> (Int, [Int])
step [day1, day2, day3, day4, day5, day6, day7, day8, day9] = next
    where
        next = (day1, [day2, day3, day4, day5, day6, day7, day8 + day1, day9, day1])

doDaysThingy :: Int -> (Int, [Int]) -> Int
doDaysThingy 0 (bornTotal, _) = bornTotal
doDaysThingy n (bornAlready, arr) = n'
    where 
        (bornToday, next) = step arr
        n' = doDaysThingy (n-1) (bornAlready + bornToday, next)

part2 :: Inp -> Int
part2 inp = res
    where 
        allDays = [0..8]
        daysThingy = inp
        countN n = length $ filter (==n) daysThingy
        actualDaysThingy = map countN allDays

        res = length daysThingy + doDaysThingy 256 (0, actualDaysThingy)

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
