module Main where
import Data.List
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow, traceShowId)
import qualified Data.Set as Set
import Data.Maybe (fromJust, catMaybes, mapMaybe, isNothing)
import qualified Data.Bifunctor
import qualified Data.Map as M


showDumbo :: Dumbo -> String
showDumbo (Charging i) = show i
showDumbo JustFlashed = "f"
showDumbo Flashed = "F"

showGrid :: (Int, Grid Dumbo) -> IO ()
showGrid (i, g) = do
    putStrLn $ M.foldlWithKey (\acc (r, c) d -> acc ++ (if c == 1 then "\n" else "") ++ showDumbo d ++ " ") "" g
    putStrLn $ "\n" ++ show i

type Grid a = M.Map (Int, Int) a

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

mapGridToPoint :: [[a]] -> Grid a
mapGridToPoint g = pointsGrid
    where
        indexedCols = map (zip [1..]) g
        indexedRows = zip [1..] indexedCols
        points = concatMap (\(rowIdx, row) -> map (\(colIdx, a) -> ((rowIdx, colIdx), a)) row) indexedRows
        pointsGrid = M.fromList points

mapAny :: (a -> Bool) -> M.Map b a -> Bool
mapAny pred m = (>0) $ M.size (M.filter pred m)

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        charsGrid = map (filter (not . null) . splitOn "") lns
        dumboGrid = map (map (Charging . read)) charsGrid
        inp = mapGridToPoint dumboGrid

data Dumbo = Charging Int | Flashed | JustFlashed deriving (Show)

type Coordinate = (Int, Int)
type Inp = Grid Dumbo
type StepResult = (Inp, Int)

directions :: [(Int, Int)]
directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

getNeighbors :: Inp -> Coordinate -> Inp
getNeighbors grid (r, c) = M.filterWithKey (\p2 _ -> p2 `elem` neighborCoords) grid
    where neighborCoords = map (Data.Bifunctor.bimap (r +) (c +)) directions

justFlashed :: Dumbo -> Bool
justFlashed JustFlashed = True
justFlashed _ = False

getNeighborsJustFlashedCount :: Inp -> Coordinate -> Int
getNeighborsJustFlashedCount inp p = length $ M.filter justFlashed $ getNeighbors inp p

increaseByOne :: Dumbo -> Dumbo
increaseByOne (Charging i) = Charging (i+1)
increaseByOne n = undefined

maybeFlashDumbo :: Dumbo -> Dumbo
maybeFlashDumbo (Charging i) = if i > 9 then JustFlashed else Charging i
maybeFlashDumbo n = n

increaseDumbo :: Int -> Dumbo -> Dumbo
increaseDumbo by (Charging i) = Charging (i + by)
increaseDumbo _ JustFlashed = Flashed
increaseDumbo _ Flashed = Flashed

resetDumbo :: Dumbo -> Dumbo
resetDumbo Flashed = Charging 0
resetDumbo n = n

resetGrid :: Inp -> Inp
resetGrid = M.map resetDumbo

step :: Inp -> StepResult
step dumbos = res
    where
        increasedDumbo = M.map increaseByOne dumbos
        res = flashLoop (increasedDumbo, 0)

flashLoop :: StepResult -> StepResult
flashLoop (dumbos, totalFlashed) = if mapAny justFlashed flashedDumbos
                                    then flashLoop (chainReaction, totalFlashed + flashedThisRoundCount) 
                                    else (resetGrid flashedDumbos, totalFlashed) 
    where
        flashedDumbos = M.map maybeFlashDumbo dumbos
        flashedThisRoundCount = length $ M.filter justFlashed flashedDumbos
        chainReaction = M.mapWithKey (increaseDumbo . getNeighborsJustFlashedCount flashedDumbos) flashedDumbos

part1 :: Inp -> Int
part1 inp = res
    where
        res = snd $ foldl (\(g, acc) _ -> let (newG, c) = step g in (newG, acc + c)) (inp, 0) [1..100]

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

part2 :: Inp -> Int
part2 inp = let (idx, _, _) = last stepShtut in idx + 1
    where
        steps = scanl (\(_, g, _) i -> let (newG, c) = step g in (i, newG, c)) (0, inp, 0) [1..]
        stepShtut = takeWhile (\(_, _, flashed) -> flashed < 100) steps

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
