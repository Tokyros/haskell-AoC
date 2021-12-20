module Main where
import Data.List
import Data.List.Split ( splitOn )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe, isJust, fromJust)
import MyUtils (Grid, from2dList)
import qualified Control.Arrow as Data.Bifunctor
import Text.ParserCombinators.ReadPrec (reset)
import qualified Text.ParserCombinators.ReadP as M
-- import qualified Data.Heap as H

import Control.Monad.State.Lazy
import qualified Data.Bifunctor


readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = inp
    where
        inp = from2dList (map (map read . tail . splitOn "") (lines str))

type Inp = Grid Int
type MeM = State (M.Map (Int, Int) Int) Int

getMinimalNeighbor :: (Int, Int) -> Grid Int -> Inp -> Int
getMinimalNeighbor (r, c) mem inp = res
    where
        a = fromMaybe 0 $ M.lookup (r, c + 1) mem
        b = fromMaybe 0 $ M.lookup (r + 1, c) mem
        f = filter (> 0) [a, b]
        n = if null f then 0 else minimum f
        res = n

getMinimalNeighbor' :: (Int, Int) -> Grid Int -> Inp -> Int
getMinimalNeighbor' (r, c) mem inp = res
    where
        a = fromMaybe 0 $ M.lookup (r, c - 1) mem
        b = fromMaybe 0 $ M.lookup (r - 1, c) mem
        f = filter (> 0) [a, b]
        n = if null f then 0 else minimum f
        res = n

takeLowest :: Maybe Int -> Int -> Int
takeLowest Nothing i = i
takeLowest (Just i) z = minimum [i, z]

data N = N Int | Infinity deriving (Ord, Show, Eq)

getMinimumFromQueue :: Grid N -> S.Set (Int, Int) -> (Int, Int)
getMinimumFromQueue grid = minimumBy (\p1 p2 -> M.lookup p1 grid `compare` M.lookup p2 grid)
    -- minimumBy (\p1 p2 -> M.lookup p1 grid `compare` M.lookup p2 grid)

getNeighbors :: Grid Int -> (Int, Int) -> [(Int, Int)]
getNeighbors grid (row, col) = filter (`M.member` grid) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]

nLookup :: (Int, Int) -> Grid N -> N
nLookup p g = fromMaybe Infinity $ M.lookup p g

nAdd :: N -> N -> N
nAdd Infinity _ = Infinity
nAdd _ Infinity = Infinity
nAdd (N a) (N b) = N (a + b)

dijkstra :: Grid Int -> (Int, Int) -> Grid N
dijkstra grid from = f (M.insert from (N 0) (M.map (const Infinity) grid)) (S.fromList $ M.keys grid)
    where
        nGrid = M.map N grid
        f :: Grid N -> S.Set (Int, Int) -> Grid N
        f dist q = res dist (S.map (\p -> (nLookup p dist, p)) q) q
            where
                res :: Grid N -> S.Set (N, (Int, Int)) -> S.Set (Int, Int) -> Grid N
                res dist qWithDists q = if S.null qWithDists then dist else r
                    where
                        -- u = head $ S.toList q
                        -- qWithDists = S.map (\p -> (nLookup p dist, p)) q
                        ((n, u), _) = fromJust $ S.minView qWithDists
                        updatedQ = S.delete u q
                        neighbors = filter (`S.member` q) $ getNeighbors grid u
                        distU = nLookup u dist
                        updatedDist = foldl (\d v -> let alt' = distU `nAdd` nLookup v nGrid in M.insertWith min v alt' d) dist neighbors
                        updatedNeighbors = map (\n1 -> (n1, nLookup n1 updatedDist)) neighbors
                        updatedQWithdists = S.delete (n, u) $ S.union qWithDists (S.fromList $ map (\(a, b) -> (b, a)) updatedNeighbors)
                        r =  res updatedDist updatedQWithdists updatedQ


-- safestPath :: Inp -> (Int, Int) -> (Int, Int) -> MeM
-- safestPath grid from to = do

--     let lastRow = [ (fst to, col) | col <- reverse [1..snd to] ]
--     let cols = concatMap (\(row, col) -> [ (r, col) | r <- reverse [1..row] ]) lastRow
--     mapM_ (\(r, c) -> modify (\s -> M.insert (r, c) (getMinimalNeighbor (r, c) s grid + fromMaybe 0 (M.lookup (r, c) grid)) s)) cols

--     let lastRow' = [ (fst to, col) | col <- [1..snd to] ]
--     let cols' = concatMap (\(row, col) -> [ (r, col) | r <- [1..row] ]) lastRow'
--     foldl (\s (r, c) -> M.insert (r, c) (takeLowest (M.lookup (r,c) s) (getMinimalNeighbor' (r, c) s grid + fromMaybe 0 (M.lookup (r, c) grid))) s) M.empty cols'

--     return 5

getN :: N -> Int
getN (N a) = a
getN Infinity = -999

part1 :: Inp -> Int
part1 inp = res
    where
        -- (m, safest) = safestPath M.empty (1, 1) (100, 100) [] inp
        -- res = sum $ map (\k -> fromMaybe 0 $ M.lookup k inp) safest
        -- end = maximum $ M.keys inp
        -- s = execState (safestPath inp (1, 1) end) M.empty
        -- res = traceShow (M.lookup (1, 1) s, M.lookup (1, 2) s, M.lookup (2, 1) s) 5
        res = getN $ nLookup (100, 100) $ dijkstra inp (1, 1)

generateTile :: Grid Int -> (Int, Int) -> Grid Int
generateTile orig (x, y) = res
    where
        toAdd = x + y
        baseRow = x * 100
        baseCol = y * 100
        res = M.mapKeys (Data.Bifunctor.bimap (baseRow +) (baseCol +)) $ M.map (\a -> if (a + toAdd) `mod` 9 == 0 then 9 else (a + toAdd) `mod` 9) orig

part2 :: Inp -> Int
part2 inp = res
    where
        tiles = [ (x, y) | x <- [0..4], y <- [0..4] ]
        gridTiles = map (generateTile inp) tiles
        concated = M.fromList $ concatMap M.toList gridTiles
        onon = (1, 1)
        res = getN $ nLookup (500, 500) $ dijkstra concated onon

main :: IO ()
main = do
    inp <- readInputFile

    -- 752 - too high
    -- print $ part1 inp
    -- 3048 -- too high
    print "before"
    print $ part2 inp
