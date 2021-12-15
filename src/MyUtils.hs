module MyUtils (Grid, from2dList, windows) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Control.Applicative
import qualified Data.List

type Grid a = M.Map (Int, Int) a

from2dList :: [[a]] -> Grid a
from2dList g = pointsGrid
    where
        indexedCols = map (zip [1..]) g
        indexedRows = zip [1..] indexedCols
        points = concatMap (\(rowIdx, row) -> map (\(colIdx, a) -> ((rowIdx, colIdx), a)) row) indexedRows
        pointsGrid = M.fromList points

mapAny :: (a -> Bool) -> M.Map b a -> Bool
mapAny pred m = (>0) $ M.size (M.filter pred m)


transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . Data.List.tails