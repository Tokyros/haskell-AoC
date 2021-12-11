module Main where
import Data.List
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow, traceShowId)
import qualified Data.Set as Set
import Data.Maybe (fromJust, catMaybes, mapMaybe, isNothing)
import qualified Data.Bifunctor


showDumbo :: Dumbo -> String
showDumbo (Charging i) = show i
showDumbo JustFlashed = "f"
showDumbo Flashed = "F"

showGrid :: (Int, Inp) -> IO ()
showGrid (i, g) = do
    putStrLn $ foldl (\acc ((r, c), d) -> acc ++ (if c == 1 then "\n" else "") ++ showDumbo d ++ " ") "" g
    putStrLn $ "\n" ++ show i

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

mapGridToPoint :: [[a]] -> [(Coordinate, a)]
mapGridToPoint g = points
    where
        indexedCols = map (zip [1..]) g
        indexedRows = zip [1..] indexedCols
        points = concatMap (\(rowIdx, row) -> map (\(colIdx, a) -> ((rowIdx, colIdx), a)) row) indexedRows

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        charsGrid = map (filter (not . null) . splitOn "") lns
        dumboGrid = map (map (Charging . read)) charsGrid
        inp = mapGridToPoint dumboGrid

data Dumbo = Charging Int | Flashed | JustFlashed deriving (Show)

type Coordinate = (Int, Int)
type Point = (Coordinate, Dumbo)
type Inp = [Point]

isAdjacent :: Coordinate -> Coordinate -> Bool
isAdjacent (r1, c1) (r2, c2) = (r1, c1) /= (r2, c2) && abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1

getNeighbors :: Inp -> Point -> [Point]
getNeighbors grid (p1, _) = filter (\(p2, _) -> isAdjacent p1 p2) grid

justFlashed :: Dumbo -> Bool
justFlashed JustFlashed = True
justFlashed _ = False

getNeighborsJustFlashed :: Inp -> Point -> [Point]
getNeighborsJustFlashed inp p = filter (justFlashed . snd) $ getNeighbors inp p

getNeighborsJustFlashedCount :: Inp -> Point -> Int
getNeighborsJustFlashedCount inp p = length $ getNeighborsJustFlashed inp p

increaseByOne :: Point -> Point
increaseByOne (p, Charging i) = (p, Charging (i+1))
increaseByOne n = undefined

maybeFlash :: Point -> Point
maybeFlash (p, Charging i) = if i > 9 then (p, JustFlashed) else (p, Charging i)
maybeFlash n = n

increaseDumbo :: Inp -> Point -> Point
increaseDumbo grid p@((row, col), Charging i) = ((row, col), Charging (i + getNeighborsJustFlashedCount grid p))
increaseDumbo grid ((row, col), JustFlashed) = ((row, col), Flashed)
increaseDumbo grid ((row, col), Flashed) = ((row, col), Flashed)

resetDumbo :: Dumbo -> Dumbo
resetDumbo Flashed = Charging 0
resetDumbo n = n

resetGrid :: Inp -> Inp
resetGrid = map (Data.Bifunctor.second resetDumbo)

step :: Inp -> (Int, Inp)
step dumbos = res
    where
        increasedDumbo = map increaseByOne dumbos
        res = step' (0, increasedDumbo)

step' :: (Int, Inp) -> (Int, Inp)
step' (acc, dumbos) = res
    where
        maybeFlashedDumbos = map maybeFlash dumbos
        hasAnyJustFlashed = any (justFlashed . snd) maybeFlashedDumbos
        next = map (increaseDumbo maybeFlashedDumbos) maybeFlashedDumbos
        countFlashed = length $ filter (justFlashed . snd) maybeFlashedDumbos
        res = if hasAnyJustFlashed then step' (acc + countFlashed, next) else (acc, resetGrid maybeFlashedDumbos)

part1 :: Inp -> Int
part1 inp = res
    where
        res = fst $ foldl (\(acc, g) _ -> let (c, newG) = step g in (acc + c, newG)) (0, inp) [1..100]

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

part2 :: Inp -> Int
part2 inp = res
    where
        something = scanl (\(_, _, g) i -> let (c, newG) = step g in (i, c, newG)) (0, 0, inp) [1..]
        stepShtut = takeWhile (\(_, acc, _) -> acc < 100) something
        res = fst3 (last stepShtut) + 1

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
