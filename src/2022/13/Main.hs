{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Char as Char
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Debug.Trace (traceShow)
import Text.Printf (printf)
import Data.Maybe (fromJust)
import MyUtils (from2dList, Grid)
import Control.Monad.State (State, MonadState (put))
import qualified Data.Set as S
import Text.Parsec (parserTrace)
import Text.Parsec.Token (GenTokenParser(symbol))
import Data.List (sortBy)
import Data.List.Split

data Value = Value Int | Arr [Value] deriving (Show, Eq, Read)
type Tup = (Value, Value)

parseGenericArray :: P.Parsec String () a -> P.Parsec String () [a]
parseGenericArray convertItem =
    P.between (P.char '[') (P.char ']') (P.sepBy convertItem (P.char ','))

parseInt :: P.Parsec String () Value
parseInt = do
    num <- P.many1 P.digit
    return (Value $ read num)

parseArr :: P.Parsec String () Value
parseArr = do
    innerValues <- parseGenericArray parseValue
    return $ Arr innerValues

parseValue :: P.Parsec String () Value
parseValue =
    parseInt P.<|> parseArr

parseTup :: P.Parsec String () Tup
parseTup = do
    a <- parseValue
    P.char '\n'
    b <- parseValue
    P.char '\n'
    return (a, b)

parseTups :: P.Parsec String () [Tup]
parseTups =
    P.sepBy parseTup (P.char '\n')

parse :: String -> [Tup]
parse st = res
    where
        parsed = P.parse parseTups "Source" st
        res = case parsed of
            Right tups -> tups;
            Left err -> traceShow err [];

compareValues :: Value -> Value -> Ordering
compareValues (Value x) (Value y) = x `compare` y
compareValues a@(Arr x) b@(Value y) = compareValues a (Arr [b])
compareValues a@(Value x) b@(Arr y) = compareValues (Arr [a]) b
compareValues a@(Arr x) b@(Arr y) = res
    where
        maybeUnequal = dropWhile (== EQ) $ zipWith compareValues x y
        res = if null maybeUnequal then length x `compare` length y else head maybeUnequal

fromArr :: [Int] -> Value
fromArr xs = Arr (map Value xs)

filterBySnd :: (a -> Bool) -> [(b, a)] -> [(b, a)]
filterBySnd fn = filter (\(b, a) -> fn a)

indexify = zip [1..]

part1 :: [Tup] -> Int
part1 tups = res
    where
        packetsComparison = indexify $ map (uncurry compareValues) tups
        res = sum $ map fst $ filterBySnd (== LT) packetsComparison

dividerPackets :: [Value]
dividerPackets = [Arr [Value 6], Arr [Value 2]]

spreadTuples :: [(a, a)] -> [a]
spreadTuples = concatMap (\(a, b) -> [a, b])

part2 :: [Tup] -> Int
part2 tups = res
    where
        withDividerPackets = dividerPackets ++ spreadTuples tups
        res = product $ map fst $ filterBySnd (`elem` dividerPackets) $ indexify $ sortBy compareValues withDividerPackets

main :: IO ()
main = do
    contents <- readFile "./src/2022/13/input.txt"

    let tups = parse contents

    print $ part1 tups
    print $ part2 tups

