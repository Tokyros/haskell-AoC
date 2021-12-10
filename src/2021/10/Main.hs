module Main where
import Data.List
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow, traceShowId)
import qualified Data.Set as Set
import Data.Maybe (fromJust, catMaybes, mapMaybe, isNothing)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = inp
    where
        inp = lines str

isOpeningChar :: Char -> Bool
isOpeningChar ch = ch `elem` "({[<"

isClosingChar :: Char -> Bool
isClosingChar ch = ch `elem` ")}]>"

getOpposite :: Char -> Char
getOpposite '(' = ')'
getOpposite '{' = '}'
getOpposite '[' = ']'
getOpposite '<' = '>'
getOpposite _ = 'N'

getPoints :: Char -> Int
getPoints ')' = 3
getPoints ']' = 57
getPoints '}' = 1197
getPoints '>' = 25137
getPoints _ = undefined

getPoints2 :: Char -> Int
getPoints2 ')' = 1
getPoints2 ']' = 2
getPoints2 '}' = 3
getPoints2 '>' = 4
getPoints2 _ = undefined

foldFunc :: Char -> (String, Maybe Char) -> (String, Maybe Char)
foldFunc _ (stack, Just ch) = (stack, Just ch)
foldFunc ch ([], Nothing)
    | isOpeningChar ch = ([getOpposite ch], Nothing)
    | otherwise = ([], Just ch)
foldFunc ch (ls@(x:xs), Nothing)
    | isOpeningChar ch =  (getOpposite ch:ls, Nothing)
    | ch == x = (xs, Nothing)
    | otherwise = ([], Just ch)

getIncorrectChar :: String -> Maybe Char
getIncorrectChar st = ch
    where
        stack = foldl (flip foldFunc) ("", Nothing) st
        ch = snd stack

getIncorrectState :: String -> (String, Maybe Char)
getIncorrectState st = ch
    where
        stack = foldl (flip foldFunc) ("", Nothing) st
        ch = stack

type Inp = [String]

part1 :: Inp -> Int
part1 inp = res
    where
        bad = mapMaybe getIncorrectChar inp
        res = sum (map getPoints bad)

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l

part2 :: Inp -> Int
part2 inp = res
    where
        maybeIncorrect = map getIncorrectState inp
        incomplete = map fst $ filter (\(s, m) -> isNothing m) maybeIncorrect
        scores = map (map getPoints2) incomplete
        calculatedScores = map (foldl (\acc sc -> (acc*5) + sc) 0) scores
        sortedScores = sort calculatedScores
        res = head $ middle sortedScores

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
