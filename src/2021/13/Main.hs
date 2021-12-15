module Main where
import Data.List
import Data.List.Split ( splitOn )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe)
import MyUtils (Grid, from2dList)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        (points, "":instructions) = break (== "") lns
        arr = map (splitOn "=" . last . words) instructions
        parsedInsts = map (\[c, n] -> (head c, read n)) arr :: [Instruction]
        parsedPoints = map (map read . splitOn ",") points :: [[Int]]
        parsedPointsTuples = map (\[x, y] -> (x, y)) parsedPoints
        inp = (parsedPointsTuples, parsedInsts)

type Instruction = (Char, Int)
type Inp = ([(Int, Int)], [Instruction])

flip :: Int -> Int -> Int
flip n around = n - (abs (n - around) * 2)

part1 :: Inp -> Int
part1 inp = res
    where
        (nums, instructions) = inp
        yInstructions = [head instructions]
        flipped = traceShow yInstructions $ foldl (\n (c, i) -> nub $ map (\(x, y) -> if (if c == 'x' then x else y) <= i then (x, y) else (if c == 'y' then (x, Main.flip y i) else (Main.flip x i, y))) n) nums yInstructions
        res = traceShow flipped $  length (nub flipped)

part2 :: Inp -> Int
part2 inp = res
    where
        (nums, instructions) = inp
        yInstructions = instructions
        flipped = foldl (\n (c, i) -> nub $ map (\(x, y) -> if (if c == 'x' then x else y) <= i then (x, y) else (if c == 'y' then (x, Main.flip y i) else (Main.flip x i, y))) n) nums yInstructions

        maxX = maximum (map fst flipped)
        maxY = maximum (map fst flipped)
        allPoints = [(x, y) | x <- [0..maxX], y <- [0..maxY]]

        chars = map (\p -> if p `elem` flipped then '#' else '.' ) allPoints

        res = traceShow chars $ length (nub flipped)

main :: IO ()
main = do
    inp <- readInputFile

    -- 846 too high
    -- 636 too low
    -- print $ part1 inp
    print $ part2 inp
