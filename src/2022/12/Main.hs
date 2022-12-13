{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Char as Char
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Debug.Trace (traceShow)
import Text.Printf (printf)
import Data.Maybe (fromJust)
import MyUtils (from2dList, Grid)
import Control.Monad.State (State, MonadState (put))
import qualified Data.Set as S

getNeighboringPoints :: (Int, Int) -> [(Int, Int)]
getNeighboringPoints (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

charDistance :: Char -> Char -> Int
charDistance c1 c2 = Char.ord c1 - Char.ord c2

constructFromParentMap :: M.Map (Int, Int) (Int, Int) -> (Int, Int) -> [(Int, Int)]
constructFromParentMap map point = go [] map point
    where
        go path map point = res
            where
                newPath = if point `M.member` map then (M.!) map point : path else path
                res = if point `M.member` map then go newPath map ((M.!) map point) else path


bfs :: Grid Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
bfs grid from to = go [] [from] to []
    where
        go _ [] _ path = path
        go visited (first:rest) to path = res
            where
                newVisited = first:visited

                validNeighbors = filter (\point -> point `M.member` grid && notElem point rest && notElem point newVisited && charDistance ((M.!) grid point) ((M.!) grid first) <= 1) (getNeighboringPoints first)
                newQueue = rest ++ validNeighbors

                res
                  | first == to = path ++ [first]
                  | otherwise = go newVisited newQueue to (path ++ [first])

main :: IO ()
main = do
    contents <- readFile "./src/2022/12/input.txt"

    let cells = lines contents
    let a = from2dList cells

    let start = fst $ head $ M.toList $ M.filter (== 'S') a
    let end = fst $ head $ M.toList $ M.filter (== 'E') a

    let b = M.union (M.fromList [(start, 'a'), (end, 'z')]) a
    -- print end
    print $ length (bfs b start end)

