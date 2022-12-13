module MyUtils (Grid, from2dList, windows, parseArray, parse) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Control.Applicative
import qualified Data.List
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P
import Debug.Trace (traceShow)

type Node = Int
type Weight = Int
type Graph = Map.Map Node [(Node, Weight)]

parseArray :: P.Parsec String () a -> P.Parsec String () [a]
parseArray convertItem =
    P.between (P.char '[') (P.char ']') (P.sepBy convertItem (P.char ','))

parse :: P.Parsec String () a -> String -> a
parse parser st = res
    where
        parsed = P.parse parser "Source" st
        res = case parsed of
            Right tups -> tups;
            Left err -> traceShow err undefined;

dijkstra :: Graph -> Node -> Node -> Maybe [Node]
dijkstra graph source target = go (Map.singleton source 0) [source] []
  where
    go distances visited [] =
      let (current, _) = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) (Map.toList distances)
      in if current == target
           then Just (reverse (target : visited))
           else Nothing

    go distances visited queue =
      let (current, _) = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) (Map.toList distances)
          neighbours = fromMaybe [] (Map.lookup current graph)
          unvisited = filter (\n -> notElem n visited) (map fst neighbours)
          queue' = queue ++ unvisited
          distances' =
            foldl
              (\m (n, w) ->
                 let d = fromMaybe maxBound (Map.lookup current distances) + w
                 in if d < fromMaybe maxBound (Map.lookup n m)
                      then Map.insert n d m
                      else m)
              distances
              neighbours
      in go distances' (current : visited) queue'


type Grid a = M.Map (Int, Int) a

from2dList :: [[a]] -> Grid a
from2dList g = pointsGrid
    where
        indexedCols = map (zip [0..]) g
        indexedRows = zip [0..] indexedCols
        points = concatMap (\(rowIdx, row) -> map (\(colIdx, a) -> ((rowIdx, colIdx), a)) row) indexedRows
        pointsGrid = M.fromList points

mapAny :: (a -> Bool) -> M.Map b a -> Bool
mapAny pred m = (>0) $ M.size (M.filter pred m)


transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . Data.List.tails