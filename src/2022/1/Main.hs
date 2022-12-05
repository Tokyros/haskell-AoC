module Main where

import Data.List
import Data.List.Split
import Control.Monad.Identity (Identity)

import qualified Text.Parsec as Parsec
import qualified Control.Applicative as ApplicativeParse
import Text.Parsec ((<|>))

parse :: Parsec.Parsec String () a -> String -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

eol :: Parsec.Parsec String () Char
eol = Parsec.char '\n'

line :: Parsec.Parsec String () String
line = do
    Parsec.manyTill Parsec.anyChar eol

parserOfGroups :: Parsec.Parsec String () [[Int]]
parserOfGroups = do
    st <- ApplicativeParse.some line `Parsec.endBy` Parsec.oneOf "\nD"
    Parsec.eof
    return $ map (map read) st

myParse st = map (sum . (map read . splitOn "\n")) (splitOn "\n\n" st)
main = do
    contents <- readFile "./src/2022/1/input.txt"
    let groups = parse parserOfGroups (contents ++ "\n")

    case groups of
        Right g -> print $ maximum $ map sum g
        Left err -> print err

    case groups of
        Right g -> print $ sum . take 3 . reverse . sort . map sum $ g
        Left err -> print err

    print $ maximum $ myParse contents
    print $ sum . take 3 . reverse . sort $ myParse contents