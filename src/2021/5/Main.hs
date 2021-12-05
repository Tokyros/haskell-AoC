module Main where
import Data.List
import Data.List.Split ( splitOn )
import Control.Monad (guard)
import Debug.Trace ( traceShow )

type Point = (Int, Int)
type Line = [Point]

getFromTo :: String -> (Point, Point)
getFromTo st = (p1, p2)
    where
        [fromSt, toSt] = splitOn " -> " st
        [fromX, fromY] = map read $ splitOn "," fromSt
        [toX, toY] = map read $ splitOn "," toSt
        p1 = (fromX, fromY)
        p2 = (toX, toY)

readDiagLine :: (Point, Point) -> [Point]
readDiagLine (p1, p2) = line
    where
        (fromX, fromY) = p1
        (toX, toY) = p2
        xs = if fromX > toX then reverse [toX..fromX] else [fromX..toX]
        ys = if fromY > toY then reverse [toY..fromY] else [fromY..toY]
        line = zip xs ys

        

readLine :: (Point, Point) -> [Point]
readLine (p1, p2) = line
    where
        (fromX, fromY) = p1
        (toX, toY) = p2
        xs = if fromX > toX then [toX..fromX] else [fromX..toX]
        ys = if fromY > toY then [toY..fromY] else [fromY..toY]
        line = [ (x, y) | x <- xs, y <- ys ]

readInputFile :: IO String
readInputFile = readFile "./input.txt"

part1 :: [[Point]] -> Int
part1 inp = 5

part2 :: [[Point]] -> Int
part2 inp = 5

isHorOrVer :: (Point, Point) -> Bool
isHorOrVer ((xFrom, yFrom), (xTo, yTo)) = xFrom == xTo || yFrom == yTo

isDiag :: (Point, Point) -> Bool
isDiag ((xFrom, yFrom), (xTo, yTo)) = abs (xFrom - xTo) == abs (yFrom - yTo)

main :: IO ()
main = do
    contents <- readInputFile
    let lns = lines contents
    let horVerFromTos = filter isHorOrVer (map getFromTo lns)
    let diagLines = filter isDiag (map getFromTo lns)
    -- print diagLines
    let lines = map readLine horVerFromTos
    let linesDiag = map readDiagLine diagLines
    let all = lines ++ linesDiag
    print $ length $ filter (\gr -> length gr >= 2) $ group $ sort (concat all)

    -- print $ part1 lines
    -- print $ part2 lines
