module Main where
import Data.List ( foldl', transpose )
import Data.Char ( digitToInt, intToDigit )

{- 
    AoC 2021 🎄
    --- Day 3: Binary Diagnostic ---

    Credits for the inspiration goes to - https://github.com/amalloy/aoc-2021
    who apparently streams AoC solutions in Haskell O: - https://www.youtube.com/watch?v=Dn7xEJ3Zjfg
-}

readInputFile :: IO String
readInputFile = readFile "./input.txt"

readNums :: String -> [Int]
readNums st = map read (lines st)

{- 
    Freq represents a frequency count of zeroes and ones

    It is an instance of Semigroup so we can make it a Monoid
    It is an instance of Monoid so we can use it together with `<>`

    `<>` helps us use Freq with foldMap
    see calculateFreq to see that in action :)
-}
data Freq = Freq {zeroes, ones :: Int} deriving (Show)

instance Semigroup Freq where
    Freq o z <> Freq o' z' = Freq (o + o') (z + z')

instance Monoid Freq where
    mempty = Freq 0 0

{-
    We create a new data type to represent a Bit.
    This will help us with pattern matching and keeping track on bit logic.

    We also created a type alias of BinNum
    this is to help point out when we're dealing with a full number and not just a single bit.
-}
data Bit = Zero | One deriving (Ord, Eq, Enum, Show)
type BinNum = [Bit]

-- Bit utils
flipBit :: Bit -> Bit
flipBit One = Zero
flipBit Zero = One

toFreq :: Bit -> Freq
toFreq Zero = Freq 1 0
toFreq One = Freq 0 1
    
-- BinNum utils
stringToBinNum :: String -> BinNum
stringToBinNum = map (toEnum . digitToInt)

binNumToString :: BinNum -> String
binNumToString = map (intToDigit . fromEnum)

binToDec :: BinNum -> Int
binToDec = foldl' (\acc x -> acc * 2 + x) 0 . map fromEnum

{-
    This is where Freq being a Monoid comes into play
    Thanks to the fact that `mempty` is defined, foldMap does not require a starting value.
    Thanks to the fact that `<>` is defined, foldMap will know how to concatenate frequencies.
    In our case concatenating two frequencies just means adding the frequencies together

    e.g. (Freq 3 4) <> (Freq 6 5) == (Freq 9 9)
    e.g. (Freq 3 4) <> mempty == (Freq 3 4)
    e.g. (Freq 3 4) <> (Freq 1 2) <> Freq (1 1) == (Freq 5 7)
-}
calculateFreq :: String -> Freq
calculateFreq = foldMap toFreq . stringToBinNum

freqToBit :: Freq -> Bit
freqToBit freq
  | zCount > oCount = Zero
  | otherwise  = One
  where
      zCount = zeroes freq
      oCount = ones freq

calculateGamma :: [String] -> BinNum
calculateGamma = map (freqToBit . calculateFreq) . transpose

findRating :: Bool -> Int -> [String] -> [String]
findRating _ _ [x] = [x]
findRating co2OrOx col sts = res
    where
        binNums = map stringToBinNum sts
        (b:_) = calculateGamma (map (drop col) sts)
        compFunc = if co2OrOx then (/=) else  (==) :: Bit -> Bit -> Bool
        matching = filter (\binNum -> (binNum !! col) `compFunc` b) binNums
        reSts = map binNumToString matching
        res = findRating co2OrOx (col + 1) reSts

findCo2Rating :: [String] -> Int
findCo2Rating = binToDec . stringToBinNum . head . findRating True 0

findOxygenRating :: [String] -> Int
findOxygenRating = binToDec . stringToBinNum . head . findRating False 0

-- Solutions
part1 :: [String] -> Int
part1 lns = binToDec gamma * binToDec epsilon
    where
        gamma = calculateGamma lns
        epsilon = map flipBit gamma

part2 :: [String] -> Int
part2 lns = findCo2Rating lns * findOxygenRating lns

main :: IO ()
main = do
    contents <- readInputFile
    let lns = lines contents

    print $ part1 lns
    print $ part2 lns
