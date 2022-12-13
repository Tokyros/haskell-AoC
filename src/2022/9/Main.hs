module Day9 (part1, part2) where

import Data.List
import qualified Data.Map as M
import qualified Text.Parsec as P
import Debug.Trace (traceShow)

type Point = (Int, Int)
type History = [Point]

movePoint :: Point -> Char -> Point
movePoint (x, y) 'U' = (x, y + 1)
movePoint (x, y) 'D' = (x, y - 1)
movePoint (x, y) 'R' = (x + 1, y)
movePoint (x, y) 'L' = (x - 1, y)
movePoint _ _ = undefined

parseCommand :: P.Parsec String (Int, Int) [Point]
parseCommand = do
    direction <- P.anyChar
    P.char ' '
    howMany <- P.manyTill P.digit (P.char '\n')
    currentPoint <- P.getState
    let pointsVisited = take (read howMany) . tail $ iterate (`movePoint` direction) currentPoint
    P.setState (last pointsVisited)
    return pointsVisited

parseHistory :: P.Parsec String (Int, Int) History
parseHistory = do
    histories <- P.many parseCommand
    return (concat histories)

parse :: String -> History
parse st = case P.runParser parseHistory (0, 0) "" st
                of Right history -> (0, 0) : history;
                    Left err -> traceShow err []

follow :: Point -> Point -> Point
follow tail@(tailX, tailY) head@(headX, headY) = res
    where
        xDiff = headX - tailX
        yDiff = headY - tailY

        xIsFar = abs xDiff > 1
        yIsFar = abs yDiff > 1
        notFar = not (xIsFar || yIsFar)

        newX = if abs xDiff > 1
                then tailX + xDiff + (if xDiff < 0 then 1 else -1)
                else headX

        newY = if abs yDiff > 1
                then tailY + yDiff + (if yDiff < 0 then 1 else -1)
                else headY

        res = if notFar then tail else (newX, newY)

listFollow :: [Point] -> Point -> [Point]
listFollow tailHistory headLocation = tailHistory ++ [currentPoint `follow` headLocation]
    where
        currentPoint = last tailHistory

followList :: History -> History
followList history = foldl listFollow [head history] history

part1 :: History -> Int
part1 history = length . group . sort $ followList history

part2 :: History -> Int
part2 history = length . group . sort $ foldl (\previousTail _ -> followList previousTail) history (replicate 9 'x')

main :: IO ()
main = do
    contents <- readFile "./src/2022/9/input.txt"
    let history = parse contents

    print $ part1 history
    print $ part2 history
