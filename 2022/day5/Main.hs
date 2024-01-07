module Main where
import AoC
import Data.Char
import Data.List

data Cargo = Cargo {
    cargoCrates :: [String],
    cargoProcs  :: [String]
} deriving (Show)


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

moveCrate :: Cargo -> Int -> Int -> Cargo
moveCrate c s1 s2 = do
    let crates = cargoCrates c
    let (to_move, ns1) = splitAt 1 (crates !! s1)
    let ns2 = to_move ++ (crates !! s2)
    Cargo (replaceNth s2 ns2 (replaceNth s1 ns1 crates)) (cargoProcs c)

doProc :: Cargo -> String -> Cargo
doProc c s = do
    let (n, from, to) = parseProc s
    foldl (\c b -> moveCrate c from to) c $ replicate n 0

moveCrate2 :: Cargo -> Int -> Int -> Int -> Cargo
moveCrate2 c n s1 s2 = do
    let crates = cargoCrates c
    let (to_move, ns1) = splitAt n (crates !! s1)
    let ns2 = to_move ++ (crates !! s2)
    Cargo (replaceNth s2 ns2 (replaceNth s1 ns1 crates)) (cargoProcs c)

doProc2 :: Cargo -> String -> Cargo
doProc2 c s = do
    let (n, from, to) = parseProc s
    moveCrate2 c n from to

parseProc :: String -> (Int, Int, Int)
parseProc s = do
    let toks = split (== ' ') s
    let n = read (toks !! 1)
    let from = read (toks !! 3) - 1
    let to = read (toks !! 5) - 1
    (n, from, to)

parseStack :: Int -> [String] -> String
parseStack n (x:xs) = (if isAlpha (x !! n) then [x !! n] else []) ++ parseStack n xs
parseStack _ [] = []

parseStacks :: Int -> [String] -> [String]
parseStacks n xs
    | n >= (reverse $ sort $ map length xs) !! 0 = []
    | otherwise = [parseStack n xs] ++ parseStacks (n + 4) xs

parseInit :: [String] -> [String]
parseInit xs = parseStacks 1 xs

parseInput :: String -> Cargo
parseInput s = do
    let list = split (== "") $ lines s
    let init = list !! 0
    let procs = list !! 1
    Cargo (parseInit init) (procs)

solve1 :: Cargo -> String
solve1 c = foldl (\l s -> l ++ [s !! 0]) "" $ cargoCrates $ foldl (\c p -> doProc c p) c $ cargoProcs c

solve2 :: Cargo -> String
solve2 c = foldl (\l s -> l ++ [s !! 0]) "" $ cargoCrates $ foldl (\c p -> doProc2 c p) c $ cargoProcs c

main = do
    readFile "test.txt" >>= \inp -> print $ solve1 $ parseInput inp
    readFile "input.txt" >>= \inp -> print $ solve1 $ parseInput inp
    readFile "test.txt" >>= \inp -> print $ solve2 $ parseInput inp
    readFile "input.txt" >>= \inp -> print $ solve2 $ parseInput inp
