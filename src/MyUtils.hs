module MyUtils (Grid, from2dList) where

import qualified Data.Map as M
import Data.List.Split (splitOn)

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
