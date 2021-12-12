module Main where
import Data.List
import Data.List.Split ( splitOn )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

stringToCave :: String -> Cave
stringToCave "start" = Start
stringToCave "end" = End
stringToCave x
    | all isLower x = Small x
    | otherwise = Big x

toCaveTuple :: [String] -> (Cave, Cave)
toCaveTuple [x, y] = (stringToCave x, stringToCave y)
toCaveTuple _ = undefined

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        tups = concatMap ((\(a, b) -> [(a, [b]), (b, [a])]) . toCaveTuple . splitOn "-") lns
        caveSystem = M.fromListWith (++) tups
        inp = caveSystem

data Cave = Small String | Big String | Start | End deriving (Eq, Show, Ord)
type CaveSystem = M.Map Cave [Cave]

type Inp = CaveSystem

isSmall :: Cave -> Bool
isSmall (Small x) = True
isSmall _ = False

visitedSmallCaveTwice :: [Cave] -> Bool
visitedSmallCaveTwice caves = res
    where
        smallCaves = filter isSmall caves
        sortedSmallCaves = map length $ group $ sort smallCaves
        res = any (>1) sortedSmallCaves

walk :: ([Cave] -> (Cave -> Bool)) -> [Cave] -> Cave -> CaveSystem -> [[Cave]]
walk _ history End _ = [history ++ [End]]
walk filter history currentCave graph = res
    where
        options = fromMaybe [] (M.lookup currentCave graph)
        newHistory = history ++ [currentCave]
        modifiedGraph = case currentCave of Start -> M.delete Start graph
                                            n -> M.filterWithKey (\k v -> filter newHistory k) graph
        res = concatMap (\c -> walk filter newHistory c modifiedGraph) options

part1Filter :: [Cave] -> (Cave -> Bool)
part1Filter path c = not (isSmall c && c `elem` path)

part1 :: Inp -> Int
part1 inp = res
    where
        res = length $ walk part1Filter [] Start inp

part2Filter :: [Cave] -> (Cave -> Bool)
part2Filter path c = not (visitedSmallCaveTwice path) || not (isSmall c && c `elem` path)

part2 :: Inp -> Int
part2 inp = res
    where
        res = length $ walk part2Filter [] Start inp

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
