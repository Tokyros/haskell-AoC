module Main where
import Data.List
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow)
import qualified Data.Set as Set
import Data.Maybe (fromJust, catMaybes)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseLine :: (Int, String) -> [((Int, Int), Int)]
parseLine (idx, line) = res
    where
        cells = zip [0..] (filter (not . null) (splitOn "" line))
        res = map (\(cellIdx, cell) -> ((idx, cellIdx), read cell)) cells

parseInp :: String -> Inp
parseInp str = inp
    where
        numberedLines = zip [0..] (lines str)
        inp = concatMap parseLine numberedLines


type Inp = [((Int, Int), Int)]

getNeighbors :: (Int, Int) -> Inp -> Inp
getNeighbors (row, col) grid = catMaybes [above, below, left, right]
    where
        above = find (\(p, _) -> p == (row - 1, col)) grid
        below = find (\(p, _) -> p == (row + 1, col)) grid
        left = find (\(p, _) -> p == (row, col - 1)) grid
        right = find (\(p, _) -> p == (row, col + 1)) grid


part1 :: Inp -> Int
part1 inp = res
    where
        lowPoints = filter (\((row, col), v) -> all (\(_, n) -> v < n) (getNeighbors (row, col) inp)) inp
        riskValues = map (\(_, v) -> v + 1) lowPoints
        res = sum riskValues

constructBasinAround :: [(Int, Int)] -> ((Int, Int), Int) -> Inp -> [(Int, Int)]
constructBasinAround seen (point@(row, col), currV) grid = res
    where
        neighbors = getNeighbors point grid
        unseenNeighbors = filter (\(p, _) -> p `notElem` seen) neighbors
        relevantNeighbors = filter (\(_, v) -> v > currV && v < 9) unseenNeighbors
        newSeen = seen ++ map fst relevantNeighbors
        res = if null relevantNeighbors then seen else concatMap (\rn -> constructBasinAround newSeen rn grid) relevantNeighbors

part2 :: Inp -> Int
part2 inp = res
    where
        lowPoints = filter (\((row, col), v) -> all (\(_, n) -> v < n) (getNeighbors (row, col) inp)) inp
        basins = map (\p@((row, col), v) -> S.fromList $ constructBasinAround [(row, col)] p inp) lowPoints
        basinsBySize = map S.size basins
        top3 = take 3 $ reverse $ sort basinsBySize
        res = product top3

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
