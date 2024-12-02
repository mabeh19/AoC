module Main where

import Data.List (elemIndex)
import Data.Maybe

deleteAt :: Int -> [] a -> [] a
deleteAt = \n -> \list ->
      case n of 
          0 -> tail list
          otherwise -> head list: deleteAt (n-1) (tail list)

solve1 :: String -> IO ()
solve1 = print . length . filter (isSafe 1) . parseLevels

    where
        parseLevels = map (map read . words) . lines
        isSafe retries ls = do 
            let asc = ascending 1 3 ls
            let dec = descending 1 3 ls

            if and asc || and dec then True
            else if retries > 0 then tryPermutations ls
            else False
        tryPermutations = or . map (isSafe 0) . generatePermutations
        generatePermutations ls = map (\n -> (take n ls) ++ (drop (n + 1) ls)) [0..(length ls - 1)]
        descending _ _ [] = [True]
        descending _ _ (_:[]) = [True]
        descending lo hi (x:y:xs) = [(inRange (-hi) (-lo) (y - x))] ++ descending lo hi (y:xs)
        ascending _ _ [] = [True]
        ascending  _ _ (_:[]) = [True]
        ascending  lo hi (x:y:xs) = [(inRange lo hi (y - x))] ++ ascending lo hi (y:xs)
        inRange lo hi n = (n >= lo) && (n <= hi)


main :: IO ()
main = do
    readFile "test.txt" >>= solve1
    readFile "input.txt" >>= solve1
