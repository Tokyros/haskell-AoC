{-# LANGUAGE TupleSections #-}
module Main where
import Data.List
import Data.List.Split ( splitOn )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe, isJust)
import MyUtils (Grid, from2dList, windows)
import qualified Control.Arrow as Data.Bifunctor

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        ([template], "":instructions) = break (== "") lns
        parsedInstructions = map ((\[a, b] -> (a, head b)) . splitOn " -> ") instructions
        inp = (template, parsedInstructions)

type Inp = (String, [(String, Char)])

type PairsCount = M.Map String Integer
type ChangeMap = M.Map String Char
type CharsCount = M.Map Char Integer

instructionToTup :: (String, Char) -> [String]
instructionToTup (a:b:"", c) = [a:c:"", c:b:""]
instructionToTup _ = undefined

getInstructionChangeMap :: [(String, Char)] -> ChangeMap
getInstructionChangeMap = M.fromList

getMaybeInstruction :: String -> [(String, Char)] -> Maybe Char
getMaybeInstruction s inst = res
    where
        maybeInstruction = lookup s inst
        res = maybeInstruction

round :: Inp -> String
round (template, instructions) = res
    where
        wds = windows 2 template
        mappedWds = concatMap (\a -> case lookup a instructions of Nothing -> [head a]; Just c -> head a:[c]) wds
        res = mappedWds ++ [last template]

roundCount :: ChangeMap -> (CharsCount, PairsCount) -> (CharsCount, PairsCount)
roundCount cm (cc, pc) = (newCharsCount, pairs)
    where
        addedChars = map (Data.Bifunctor.first (fromMaybe ' ') . (\(pair, count) -> (M.lookup pair cm, count))) (M.toList pc)
        newCharsCount = M.fromListWith (+) $ addedChars ++ M.toList cc

        pairs = M.fromListWith (+) $ concatMap (\(pair@(a:b:""), count) -> case M.lookup pair cm of Just c -> [(a:c:"", count), (c:b:"", count), (pair, 0)]; Nothing -> [(pair, count)]) $ M.toList pc

part1 :: Inp -> Int
part1 inp = res
    where
        initialTemplate = fst inp
        instructions = snd inp
        roundsResult = foldl (\template _ -> Main.round (template, instructions)) initialTemplate [1..10]
        grp = map length $ group . sort $ roundsResult
        (min, max) = (minimum grp, maximum grp)
        res = max - min

part2 :: Inp -> Integer
part2 inp = res
    where
        initialTemplate = fst inp
        instructions = snd inp
        changeMap = getInstructionChangeMap instructions
        initialCharsCount = M.fromListWith (+) $ map (, 1 :: Integer) initialTemplate
        pairsCount = M.fromListWith (+) $ map (, 1) $ windows 2 initialTemplate
        withRules = roundCount changeMap
        roundsResult = foldl (\a _ -> withRules a) (initialCharsCount, pairsCount) [1..40]
        charsCount = fst roundsResult
        concatenatedCharCounts = M.toList charsCount
        max = maximum (map snd concatenatedCharCounts)
        min = minimum (map snd concatenatedCharCounts)
        res = traceShow (max, min) $ max - min

main :: IO ()
main = do
    inp <- readInputFile

    -- print $ part1 inp
    -- 14514240396154 too high
    -- 7155869758394 too low 
    -- 7832277673055 too low
    print $ part2 inp
