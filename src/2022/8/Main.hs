module Main where

import Data.List (any, foldl, find, group, sort, tails, findIndex, transpose, intercalate )
import Data.Maybe (fromJust)
import Data.Map (empty, fromList, Map, (!))
import qualified Data.Map as M
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Text.Parsec (Parsec, updateState, try, (<|>), string, char, many, many1, runParser, anyChar, digit, endOfLine, manyTill, getState, setState, optional, parserTrace)
import Debug.Trace (trace)
import Text.Printf (printf)

type Point = (Int, Int)
type TreeHeight = Int
type Tree = (Point, TreeHeight)
type Forest = M.Map Point TreeHeight

parseTree :: Parsec String Point Tree
parseTree = do
    height <- digit
    point <- getState
    updateState (\(x, y) -> (x + 1, y))
    return (point, read [height])

parseRow :: Parsec String Point [Tree]
parseRow = do
    trees <- many1 parseTree
    char '\n'
    updateState (\(x, y) -> (0, y + 1))
    return trees

parseForest :: Parsec String Point Forest
parseForest = do
    rows <- many1 parseRow
    return (M.fromList (concat rows))

parse :: String -> Forest
parse st = res
    where
        parsingResult = runParser parseForest (0, 0) "Parser" st
        res = case parsingResult of
                Right forest -> forest
                Left err -> trace (show err) M.empty

steps = [
    \(x, y) -> (x, y - 1),
    \(x, y) -> (x, y + 1),
    \(x, y) -> (x + 1, y),
    \(x, y) -> (x - 1, y)
    ]

getAtDirection :: (Point -> Point) -> Forest -> Tree -> [Tree]
getAtDirection step forest (point, height) = trees
    where
        pointsAboveMe = takeWhile (`M.member` forest) $ iterate step (step point)
        trees = map (\point -> (point, forest M.! point)) pointsAboveMe

isVisibleOutsideForest :: Forest -> Tree -> Bool
isVisibleOutsideForest forest tree@(point, treeHeight) = res
    where
        directions = map (\step -> getAtDirection step forest tree) steps
        res = any (all (\(_, height) -> height < treeHeight)) directions

calculateScenicScore :: Forest -> Tree -> Int
calculateScenicScore forest tree@(point, treeHeight) = res
    where
        above = map (\(point, h) -> h - treeHeight) $ getAtDirection (\(x, y) -> (x, y - 1)) forest tree
        below = map (\(point, h) -> h - treeHeight) $  getAtDirection (\(x, y) -> (x, y + 1)) forest tree
        right = map (\(point, h) -> h - treeHeight) $  getAtDirection (\(x, y) -> (x + 1, y)) forest tree
        left = map (\(point, h) -> h - treeHeight) $  getAtDirection (\(x, y) -> (x - 1, y)) forest tree

        idxOrLength arr = case findIndex (>= 0) arr of Just i -> i+1; Nothing -> length arr

        res = product $ map idxOrLength [above, below, right, left]

main :: IO ()
main = do
    contents <- readFile "./src/2022/8/input.txt"

    let forest = parse contents

    let trees = M.toList forest

    print $ M.size $ M.filterWithKey (curry (isVisibleOutsideForest forest)) forest
    print $ maximum $ map (\tree -> calculateScenicScore forest tree) trees
