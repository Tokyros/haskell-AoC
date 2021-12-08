module Main where
import Data.List
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow)
import qualified Data.Set as Set

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = inp
    where
        lns = lines str
        lnsSplit = map (splitOn " | ") lns
        inp = map (\[a, b] -> (words a, words b)) lnsSplit

type Inp = [([String], [String])]

count1s :: ([String], [String]) -> Int
count1s (a, b) = length (filter ((==2) . length) b)

count4s :: ([String], [String]) -> Int
count4s (a, b) = length (filter ((==4) . length) b)

count7s :: ([String], [String]) -> Int
count7s (a, b) = length (filter ((==3) . length) b)

count8s :: ([String], [String]) -> Int
count8s (a, b) = length (filter ((==7) . length) b)

part1 :: Inp -> Int
part1 inp = countOf1s + countOf4s + countOf7s + countOf8s
    where
        countOf1s = sum $ map count1s inp
        countOf4s = sum $ map count4s inp
        countOf7s = sum $ map count7s inp
        countOf8s = sum $ map count8s inp

parseLine :: ([String], [String]) -> Int
parseLine (inp, outp) = res
    where
        zero = head (filter (\noz -> Set.size (four `Set.difference` noz) /= 0) ninesAndZeros)
        one = Set.fromList $ concat $ filter ((==2) . length) inp
        two = Set.fromList [top, topRight, middle, bottomLeft, bottom]
        three = Set.fromList [top, topRight, middle, bottomRight, bottom]
        four = Set.fromList $ concat $ filter ((==4) . length) inp
        five = Set.fromList [top, topLeft, middle, bottomRight, bottom]
        six = head $ filter (\s -> length (s `Set.intersection` one) == 1) thingsWithSixChars
        seven = Set.fromList $ concat $ filter ((==3) . length) inp
        eight = Set.fromList $ concat $ filter ((==7) . length) inp
        nine = head (filter (\noz -> Set.size (four `Set.difference` noz) == 0) ninesAndZeros)

        top = Set.elemAt 0 $ seven `Set.difference` one
        bottom = Set.elemAt 0 $ nine `Set.difference` (seven `Set.union` four)
        middle = Set.elemAt 0 $ eight `Set.difference` middleMask
        bottomRight = Set.elemAt 0 $ one `Set.difference` Set.singleton topRight
        bottomLeft = Set.elemAt 0 $ zero `Set.difference` bottomLeftMask
        topLeft = Set.elemAt 0 $ zero `Set.difference` topLeftMask
        topRight = Set.elemAt 0 $ one `Set.difference` six

        thingsWithSixChars = map Set.fromList $ filter ((==6) . length) inp

        ninesAndZeros = filter (\t -> topRight `Set.member` t) thingsWithSixChars

        topLeftMask = (seven `Set.union` (Set.singleton bottom `Set.union` Set.singleton bottomLeft)) `Set.union` Set.singleton bottom
        bottomLeftMask = (four `Set.union` seven) `Set.union` Set.singleton bottom
        middleMask = Set.fromList [top, topRight, bottomRight, bottom, bottomLeft, topLeft]

        nums = [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]

        outpSet = map Set.fromList outp
        values = map (snd . (\outpCharSet -> head $ filter ((== outpCharSet) . fst) nums)) outpSet
        res = read $ intercalate "" $ map show values

part2 :: Inp -> Int
part2 inp = res
    where
        res = sum (map parseLine inp)

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
