module Main where
import Data.List
import Data.List.Split ( splitOn, chunksOf )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe, isJust)
import MyUtils (Grid, from2dList)

readInputFile :: IO Inp
readInputFile = do
    contents <- readFile "./input.txt"
    return $ parseInp contents

parseInp :: String -> Inp
parseInp str = fst inp
    where
        bin = concatMap hexToBin str
        inp = parseSinglePacket bin

binToInt :: String -> Int
binToInt = foldl step 0
    where step y x = (+) (digitToInt x) ( (*) y 2 )

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin _ = undefined

parseSinglePacket :: String -> (Packet, String)
parseSinglePacket st = (res, r)
    where
        (res, r) = case padWithZeros st of
            (v1:v2:v3:t1:t2:t3:rest) ->
                let (version, idType) = (binToInt (v1:v2:v3:""), binToInt (t1:t2:t3:"")) in
                    if idType == 4
                        then let (v, r) = parseLiteralValue rest in (Literal { _idType = idType, _v = version, value = v }, r)
                        else let (sp, r) = parseSubPackets rest in (Operator { _idType = idType, _v = version, _subPackets =  sp}, r)
            _ -> undefined

parseSubPackets :: String -> ([Packet], String)
parseSubPackets st = res
    where
        (i:rest) = st
        res = if i == '0'
            then parseSubPacketsByLengthInBits rest
            else parseSubPacketsByPacketCount rest

parseSubPacketsByPacketCount :: String -> ([Packet], String)
parseSubPacketsByPacketCount st = res
    where
        subPacketsCount = binToInt (take 11 st)
        rest = drop 11 st
        res = foldl (\(sps, st') _ -> let (p, r) = parseSinglePacket st' in (sps ++ [p], r)) ([], rest) [1..subPacketsCount]

consumeSt :: String -> [Packet]
consumeSt st
    | st == "" = []
    | all (=='0') st = []
    | otherwise = let (packet, rest) = parseSinglePacket st in packet:consumeSt rest

parseSubPacketsByLengthInBits :: String -> ([Packet], String)
parseSubPacketsByLengthInBits st = res
    where
        subPacketsLength = binToInt (take 15 st)
        rest = drop 15 st
        relevantBits = take subPacketsLength rest
        sp = consumeSt relevantBits
        res = (sp, drop subPacketsLength rest)

padWithZeros :: String -> String
padWithZeros st = st ++ replicate (length st `rem` 4) '0'

parseLiteralValue :: String -> (Int, String)
parseLiteralValue st = (res, concat rest)
    where
        chunksOfFive = chunksOf 5 st
        (relevantChunks, relevantChunksLast:rest) = span ((/='0') . head) chunksOfFive
        bin = concatMap tail (relevantChunks ++ [relevantChunksLast])
        res = binToInt bin

type Inp = Packet
data Packet = Operator { _v :: Int, _idType :: Int, _subPackets :: [Packet] }
            | Literal { _v :: Int, _idType :: Int, value :: Int }
            deriving (Show, Eq, Ord)

getVersions :: Packet -> [Int]
getVersions (Literal v _ _) = [v]
getVersions (Operator v _ sp) = v:concatMap getVersions sp

part1 :: Inp -> Int
part1 inp = res
    where
        res = sum $ getVersions inp

calculatePacket :: Packet -> Int
calculatePacket (Literal _ _ v) = v
calculatePacket (Operator _ 0 sp) = sum (map calculatePacket sp)
calculatePacket (Operator _ 1 sp) = product (map calculatePacket sp)
calculatePacket (Operator _ 2 sp) = minimum (map calculatePacket sp)
calculatePacket (Operator _ 3 sp) = maximum (map calculatePacket sp)
calculatePacket (Operator _ 5 (p1:p2:_)) = if calculatePacket p1 > calculatePacket p2 then 1 else 0
calculatePacket (Operator _ 6 (p1:p2:_)) = if calculatePacket p1 < calculatePacket p2 then 1 else 0
calculatePacket (Operator _ 7 (p1:p2:_)) = if calculatePacket p1 == calculatePacket p2 then 1 else 0
calculatePacket Operator {} = undefined

part2 :: Inp -> Int
part2 inp = res
    where
        res = calculatePacket inp

main :: IO ()
main = do
    inp <- readInputFile

    print $ part1 inp
    print $ part2 inp
