module Main where

import Data.List
import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Debug.Trace (traceShow)
import Text.Printf (printf)
import Data.Maybe (fromJust)

data Monkey = Monkey {
    starting :: [Int],
    worryFactor :: Int,
    operation :: Int -> Int,
    diviseBy :: Int,
    monkeyIfTrue :: Int,
    monkeyIfFalse :: Int,
    inspected :: Int
}

instance Show Monkey where
  show (Monkey starting worryFactor operation diviseBy monkeyIfTrue monkeyIfFalse inspected) =
    "Monkey: (inspected: " ++ show inspected ++ ") " ++ show starting ++ ", 1 <operation> " ++ show worryFactor ++ " == " ++ show (operation 2) ++ " | Divise by " ++ (show diviseBy) ++ ", if true pass to - " ++ (show monkeyIfTrue) ++ ", if false pass to - " ++ (show monkeyIfFalse) ++ "\n"

parseMonkey :: P.Parsec String () Monkey
parseMonkey = do

    P.manyTill P.anyChar (P.char '\n')
    P.string "  Starting items: "
    items <- P.sepBy (P.many P.digit) (P.string ", ")
    P.char '\n'
    P.string "  Operation: new = old "
    operator <- P.anyChar
    P.char ' '
    factor <- (P.many1 P.digit) P.<|> (P.string "old")
    P.char '\n'
    P.string "  Test: divisible by "
    diviseBy <- P.many P.digit
    P.char '\n'
    P.string "    If true: throw to monkey "
    monkeyIfTrue <- P.manyTill P.digit (P.char '\n')
    P.string "    If false: throw to monkey "
    monkeyIfFalse <- P.manyTill P.digit (P.char '\n')

    P.optional $ P.char '\n'

    let operation = if operator == '*' then (*) else (+)

    return (Monkey {
        starting = map read items,
        worryFactor = if factor == "old" then -1 else read factor,
        operation = \old -> operation (if factor == "old" then old else read factor) old,
        diviseBy = read diviseBy,
        monkeyIfTrue = read monkeyIfTrue,
        monkeyIfFalse = read monkeyIfFalse,
        inspected = 0
    })

parseMonkeys :: String -> [Monkey]
parseMonkeys st = case P.parse (P.many parseMonkey) "Monkey parser" st of Right monkeys -> monkeys; Left err -> traceShow err []

type MonkeyMap = M.Map Int Monkey

monkeysToMap :: [Monkey] -> M.Map Int Monkey
monkeysToMap monkeys = M.fromList $ zip [0..] monkeys



executeMonkey :: Int -> MonkeyMap -> MonkeyMap
executeMonkey idx monkeys = res
    where
        monkey = monkeys M.! idx
        items = starting monkey
        newValues = map ((`quot` 3) . operation monkey) items
        itemsToTransferToTrue = filter (\item -> item `mod` diviseBy monkey == 0) newValues
        itemsToTransferToFalse = filter (\item -> item `mod` diviseBy monkey /= 0) newValues

        trueMonkey = monkeys M.! monkeyIfTrue monkey
        falseMonkey = monkeys M.! monkeyIfFalse monkey

        withUpdatedTrue = M.insert (monkeyIfTrue monkey) (trueMonkey { starting = starting trueMonkey ++ itemsToTransferToTrue}) monkeys
        withUpdatedFalse = M.insert (monkeyIfFalse monkey) (falseMonkey { starting = starting falseMonkey ++ itemsToTransferToFalse}) withUpdatedTrue
        withUpdatedMonkey = M.insert idx (monkey { starting = [], inspected = inspected monkey + length items}) withUpdatedFalse
        res = withUpdatedMonkey

executeMonkeyStressed :: Int -> MonkeyMap -> MonkeyMap
executeMonkeyStressed idx monkeys = res
    where
        monkey = monkeys M.! idx
        items = starting monkey
        newValues = map ((`mod` (9699690)) . operation monkey) items
        -- newValues = map ((`mod` (diviseBy monkey)) . operation monkey) items
        itemsToTransferToTrue = filter (\item -> item `mod` diviseBy monkey == 0) newValues
        itemsToTransferToFalse = filter (\item -> item `mod` diviseBy monkey /= 0) newValues

        trueMonkey = monkeys M.! monkeyIfTrue monkey
        falseMonkey = monkeys M.! monkeyIfFalse monkey

        withUpdatedTrue = M.insert (monkeyIfTrue monkey) (trueMonkey { starting = starting trueMonkey ++ itemsToTransferToTrue}) monkeys
        withUpdatedFalse = M.insert (monkeyIfFalse monkey) (falseMonkey { starting = starting falseMonkey ++ itemsToTransferToFalse}) withUpdatedTrue
        withUpdatedMonkey = M.insert idx (monkey { starting = [], inspected = inspected monkey + length items}) withUpdatedFalse
        res = withUpdatedMonkey

executeRound :: MonkeyMap -> MonkeyMap
executeRound monkeys = res
    where
        res = foldl (flip executeMonkeyStressed) monkeys [0..(M.size monkeys - 1)]

chineseRemainder :: [Integer] -> [Integer] -> Integer
chineseRemainder residues modulii = head [ x | x <- [0..product modulii - 1],
                                                and [x `mod` m == r | (r,m) <- zip residues modulii]]

main :: IO ()
main = do
    contents <- readFile "./src/2022/11/input.txt"

    -- print $ chineseRemainder [79, 98] [23, 23]

    let monkeyMap = monkeysToMap $ parseMonkeys contents
    let rounds = foldl (\acc _ -> executeRound acc) monkeyMap [1..10000]
    -- print $ rounds
    let lcm = product $ map snd $ M.toList $ M.map diviseBy monkeyMap
    print lcm
    print $ product $ take 2 $ reverse $ sort $ map snd $ M.toList $ M.map inspected rounds
