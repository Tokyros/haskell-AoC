module Main where
import Data.List ( foldl', transpose )
import Data.List.Split ( splitOn )

data Cell = Marked Int | Unmarked Int deriving Show
type Point = (Int, Int)
type Board = [[Cell]]

createBoard :: [String] -> Board
createBoard = map createCellRow
    where
        createCellRow = map (Unmarked . read) . words

markCell :: Int -> Board -> Board
markCell n = map markRow
    where
        maybeMark c@(Unmarked i)
            | i == n = Marked i
            | otherwise = c
        maybeMark c@(Marked _) = c
        markRow = map maybeMark

isMarked :: Cell -> Bool
isMarked (Marked _) = True
isMarked (Unmarked _) = False

allMarked :: [Cell] -> Bool
allMarked = all isMarked

boardWon :: Board -> Bool
boardWon b = any allMarked (transpose b) || any allMarked b

sumUnmarked :: Board -> Int
sumUnmarked b = sum summedRows
    where
        filterUnmarked = filter (not . isMarked)
        onlyUnmarked = map filterUnmarked b
        sumRow = foldl' (\acc (Unmarked i) -> acc + i) 0
        summedRows = map sumRow onlyUnmarked

readInputFile :: IO String
readInputFile = readFile "./input.txt"

readNums :: String -> [Int]
readNums st = map read (lines st)

part1 :: Int -> [Board] -> [Int] -> Int
part1 lastPlay boards plays =
    case filter boardWon boards of
        [winningBoard] -> sumUnmarked winningBoard * lastPlay
        _ -> part1 currentPlay boardsAfterPlay remainingPlays
    where
        currentPlay = head plays
        remainingPlays = tail plays
        boardsAfterPlay = map (markCell currentPlay) boards

part2 :: Int -> [Board] -> [Int] -> Int
part2 lastPlay boards plays =
    case filter (not . boardWon) boards of
        [notWinningBoard] -> part1 lastPlay [notWinningBoard] plays
        _ -> part2 currentPlay boardsAfterPlay remainingPlays
    where
        currentPlay = head plays
        remainingPlays = tail plays
        boardsAfterPlay = map (markCell currentPlay) boards

main :: IO ()
main = do
    contents <- readInputFile
    let lns = lines contents
    let chunks = splitOn [""] lns
    let (numsInp:boardInp) = chunks
    let boards = map createBoard boardInp
    let numsSplit = splitOn "," (head numsInp)
    let nums = map read numsSplit :: [Int]

    print $ part1 0 boards nums
    print $ part2 0 boards nums
