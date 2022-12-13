module Main where

import Data.List (foldl, find, group, sort, tails, findIndex, transpose, intercalate )
import Data.Maybe (fromJust)
import Data.Map (empty, fromList, Map, (!))
import qualified Data.Map as M
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Text.Parsec (Parsec, updateState, try, (<|>), string, char, many, many1, runParser, anyChar, digit, endOfLine, manyTill, getState, setState, optional, parserTrace)
import Debug.Trace (trace)
import Text.Printf (printf)

data Descriptor = File String Int | DirectoryName String deriving (Show)
data Directory = Directory String [Descriptor] deriving (Show)
type FileSystem = [Directory]
type FileSystemMap = M.Map String [Descriptor]

fileSystemToMap :: FileSystem -> FileSystemMap
fileSystemToMap fs = fromList $ map (\(Directory name descs) -> (name, descs)) fs

parseDirectoryName :: Parsec String [String] ()
parseDirectoryName = do
    name <- manyTill anyChar (char '\n')
    updateState (\path -> path ++ [name])

parseGoBack :: Parsec String [String] ()
parseGoBack = do
    string "..\n"
    updateState init

parseChangeDirectory :: Parsec String [String] ()
parseChangeDirectory = do
    string "$ cd "
    parseGoBack <|> parseDirectoryName
    return ()

parseFile :: Parsec String [String] Descriptor
parseFile = do
    size <- many digit
    char ' '
    name <- manyTill anyChar (char '\n')
    return (File name (read size))

parseDirectoryNameInner :: Parsec String [String] Descriptor
parseDirectoryNameInner = do
    string "dir "
    name <- manyTill anyChar (char '\n')
    currentDir <- getState
    return (DirectoryName (intercalate "/" currentDir ++ "/" ++ name))


parseDirectory :: Parsec String [String] Directory
parseDirectory = do
    name <- getState
    descriptors <- many (parseDirectoryNameInner <|> parseFile)
    return (Directory (intercalate "/" name) descriptors)

parseLSStream :: Parsec String [String] Directory
parseLSStream = do
    many $ try parseChangeDirectory
    string "$ ls\n"
    directories <- parseDirectory
    s <- getState
    return directories


parseTerminal :: Parsec String [String] FileSystem
parseTerminal = do
    many1 parseLSStream


parse :: String -> FileSystem
parse st = case runParser parseTerminal [] "(Parsing Input)" st of
            Right fs -> fs
            Left _ -> []

calcDescriptor :: FileSystemMap -> Descriptor -> Int
calcDescriptor fs descriptor = case descriptor of
                                    File _ size -> size
                                    DirectoryName name -> calcDirectory fs (fs ! name)

calcDirectory :: FileSystemMap -> [Descriptor] -> Int
calcDirectory fs = foldl (\acc desc -> acc + calcDescriptor fs desc) 0

main :: IO ()
main = do
    contents <- readFile "./src/2022/7/input.txt"

    let fs = parse contents
    let fsMap = fileSystemToMap fs
    let directoryToSize = M.mapWithKey (\key val -> calcDirectory fsMap val) fsMap
    let allSizes = map snd $ M.toList directoryToSize
    let totalSize = calcDirectory fsMap (fsMap M.! "/")
    let unusedStorage = 70000000 - totalSize
    let needed = 30000000 - unusedStorage
    let usefulSizes = filter (>=needed) allSizes
    let part1Res = sum $ filter (<=100000) allSizes
    let part2Res = minimum usefulSizes

    print part1Res
    print part2Res
