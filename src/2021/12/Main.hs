module Main where
import Data.List
import Data.List.Split ( splitOn )
import qualified Data.Set as S
import qualified Data.Map as M
import MyUtils
import Data.Char
import Debug.Trace (traceShow)


readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end" = End
parseCave x
    | all isLower x = Small x
    | all isUpper x = Big x
    | otherwise = undefined

mapPairs :: [Cave] -> [Connection]
mapPairs [c1@Start, c2] = [(c1, c2)]
mapPairs [c1, c2@Start] = [(c2, c1)]
mapPairs [c1, c2@End] = [(c1, c2)]
mapPairs [c1, c2] = [(c1, c2), (c2, c1)]
mapPairs _ = undefined

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        pairs = map (map parseCave . splitOn "-") lns
        connections = concatMap mapPairs pairs
        inp = connections

type Inp = [Connection]

data Cave = Start | End | Small String | Big String deriving (Show, Eq)
type Connection = (Cave, Cave)

isStart :: Cave -> Bool
isStart Start = True
isStart _ = False

getPossibleNextSteps :: [Connection] -> Cave -> [Connection]
getPossibleNextSteps cons cave = filter (\(c1, c2) -> c1 == cave) cons

blockReturn :: Cave -> [Connection] -> [Connection]
blockReturn c@(Big x) con = con
blockReturn c@(Small x) con = filter (\(from, to) -> to /= c) con
blockReturn _ con = con

walk :: Cave -> [Connection] -> Connection -> [Connection] -> Int
walk allowed history (a, End) _ = traceShow history 1
walk allowed history _ [] = 0
walk allowed history path@(from, to) connections = res
    where
        connectionsWithoutCurrent = filter (/=path) connections
        isFromAllowedTwice = from == allowed
        withoutReturn = if not isFromAllowedTwice then blockReturn from connectionsWithoutCurrent else connectionsWithoutCurrent
        newAllowed = if not isFromAllowedTwice then allowed else Small "----------"
        nextSteps = getPossibleNextSteps withoutReturn (snd path)
        allWalks = sum $ map (\p -> walk newAllowed (history ++ [p]) p withoutReturn) nextSteps
        res = allWalks

part1 :: Inp -> Int
part1 inp = res
    where
        starts = filter (isStart . fst) inp
        res = sum (map (\s -> walk (Small "-----------") [] s inp) starts)

isSmall :: Cave -> Bool
isSmall (Small x) = True
isSmall _ = False

part2 :: Inp -> Int
part2 inp = res
    where
        allCaves = nub $ map snd inp ++ map fst inp
        smallCaves = filter isSmall allCaves

        starts = filter (isStart . fst) inp
        
        res = sum (map (\s -> sum $ map (\sc -> walk sc [] s inp) smallCaves) starts)


main :: IO ()
main = do
    inp <- readInputFile

    print inp
    print $ part1 inp
    -- print $ part2 inp
