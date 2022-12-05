module Main where

import Data.List
import Data.Char
import Data.List.Split
import Text.Parsec
import Data.Maybe (catMaybes)

parse :: Parsec String s a -> s -> String -> Either ParseError a
parse rule s = runParser rule s "(source)"

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

data Command = Command Int Int Int deriving (Show)

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

executeCommand :: Bool -> Command -> [String] -> [String]
executeCommand is9001 (Command howMany from to) stacks = res
    where
        fromStack = stacks !! from
        toStack = stacks !! to
        toMove = take howMany fromStack

        newFrom = drop howMany fromStack
        newTo = (if is9001 then toMove else reverse toMove) ++ toStack

        res =  mapInd (\idx i -> if idx == (to) then newTo else if idx == (from) then newFrom else i) stacks

executeCommands :: Bool -> [Command] -> [String] -> [String]
executeCommands _ [] stacks = stacks
executeCommands is9001 [cmd] stacks = executeCommand is9001 cmd stacks
executeCommands is9001 (cmd:cmds) stacks = executeCommands is9001 cmds (executeCommand is9001 cmd stacks)

parseCommand :: Parsec String () Command
parseCommand = do
    string "move "
    howMany <- many1 digit
    string " from "
    from <- many1 digit
    string " to "
    to <- many1 digit
    optional $ string "\n"
    return (Command (read howMany) (read from - 1) (read to - 1))

parseCommands :: Parsec String () [Command]
parseCommands = do
    many parseCommand

parseCrate :: Parsec String Int (Maybe (Int, String))
parseCrate = do
    char '['; c <- anyChar; char ']';

    s <- getState
    return (Just (s, [c]))

parseBlank :: Parsec String Int (Maybe (Int, String))
parseBlank = do
    count 3 (char ' ')
    return Nothing

parseBracket :: Parsec String Int (Maybe (Int, String))
parseBracket = do
    bracket <- parseBlank <|> parseCrate
    optional (char ' ')
    updateState (+ 1)
    return bracket

parseRow :: Parsec String Int [(Int, String)]
parseRow = do
    parserTrace "here"

    brackets <- many parseBracket
    endOfLine

    setState 0

    return (catMaybes brackets)

parseRows :: Parsec String Int [(Int, String)]
parseRows = do 
    c <- manyTill parseRow (notFollowedBy parseRow)
    return (concat c)

parseContainers :: String -> Either ParseError [[String]]
parseContainers st = do
    parsed <- runParser parseRows 0 "(Input)" st

    let numberedEntries = parsed
    let sortedEntries = sortBy (\a b -> fst a `compare` fst b) numberedEntries
    let groupedEntries = groupBy (\a b -> fst a == fst b) sortedEntries
    let stacks = map (map snd) groupedEntries
    return stacks

part1 :: Either a [[String]] -> Either a [Command] -> Either a String
part1 eitherStacks eitherCommands = do
    stacks <- eitherStacks
    commands <- eitherCommands

    let afterMoving = executeCommands False commands (map concat stacks)

    return (map head afterMoving)

part2 :: Either a [[String]] -> Either a [Command] -> Either a String
part2 eitherStacks eitherCommands = do
    stacks <- eitherStacks
    commands <- eitherCommands

    let afterMoving = executeCommands True commands (map concat stacks)

    return (map head afterMoving)

main :: IO ()
main = do
    contents <- readFile "./src/2022/5/input.txt"

    let commandsUnparsed = last $ splitOn "\n\n" contents
    let parsedCommands = Main.parse parseCommands () commandsUnparsed

    let stacksUnparsed = head $ splitOn "\n\n" contents
    let parsedStacks = parseContainers stacksUnparsed

    print $ part1 parsedStacks parsedCommands
    print $ part2 parsedStacks parsedCommands
