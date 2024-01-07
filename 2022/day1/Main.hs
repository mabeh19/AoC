module Main where
import Data.List
import AoC

solve1 :: [String] -> [Int]
solve1 s = map sum $ map (\l -> map read l) $ split (== "") s

main = readFile "test.txt" >>= (\inp -> print $ take 3 $ reverse $ sort $ solve1 $ lines inp)

