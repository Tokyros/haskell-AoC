module Main where
import Data.List
import Data.List.Split ( splitOn )

type Point = (Int, Int)
type Edges = (Point, Point)
type Line = [Point]

readInputFile :: IO String
readInputFile = readFile "./input.txt"

readCommaDelimitedNums :: String -> [Int]
readCommaDelimitedNums = map read . splitOn ","

getEdges :: String -> Edges
getEdges st = ((fromX, fromY), (toX, toY))
    where
        [fromSt, toSt] = splitOn " -> " st
        [fromX, fromY] = readCommaDelimitedNums fromSt
        [toX, toY] = readCommaDelimitedNums toSt

getRange :: Edges -> ([Int], [Int])
getRange ((fromX, fromY), (toX, toY)) = (
    if fromX > toX then reverse [toX..fromX] else [fromX..toX],
    if fromY > toY then reverse [toY..fromY] else [fromY..toY]
    )

getDiagonalLine :: Edges -> Line
getDiagonalLine edges = line
    where
        (xs, ys) = getRange edges
        line = zip xs ys

getHorizontalVerticalLine :: Edges -> Line
getHorizontalVerticalLine edges = line
    where
        (xs, ys) = getRange edges
        line = [ (x, y) | x <- xs, y <- ys ]

isHorizontalVertical :: Edges -> Bool
isHorizontalVertical ((xFrom, yFrom), (xTo, yTo)) = xFrom == xTo || yFrom == yTo

isDiagonal :: Edges -> Bool
isDiagonal ((xFrom, yFrom), (xTo, yTo)) = abs (xFrom - xTo) == abs (yFrom - yTo)

getHorizontalPoints :: [Edges] -> Line
getHorizontalPoints = concatMap getHorizontalVerticalLine . filter isHorizontalVertical

getDiagonalPoints :: [Edges] -> Line
getDiagonalPoints = concatMap getDiagonalLine . filter isDiagonal

countRepeating :: (Ord a) => Int -> [a] -> Int
countRepeating moreThan = length . filter ((>=moreThan) . length) . group . sort

part1 :: [Edges] -> Int
part1 inp = countRepeating 2 (getHorizontalPoints inp)

part2 :: [Edges] -> Int
part2 inp = countRepeating 2 (getDiagonalPoints inp ++ getHorizontalPoints inp)

main :: IO ()
main = do
    contents <- readInputFile
    let edges = map getEdges $ lines contents

    print $ part1 edges
    print $ part2 edges
