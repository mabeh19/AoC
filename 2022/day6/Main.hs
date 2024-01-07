module Main where
import Data.List

hasRepeatN :: Int -> String -> Bool
hasRepeatN n (x:xs) = not (elem x (take n xs)) && hasRepeatN (n - 1) xs
hasRepeatN 0 [] = False
