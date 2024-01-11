module Main where
import Data.List

hasRepeatN :: Int -> String -> Bool
hasRepeatN n (x:xs) = elem x (take n xs) || hasRepeatN (n - 1) xs
hasRepeatN _ [] = False

findMarker :: Int -> Int -> String -> Int
findMarker chars n (x:xs)
    | length xs >= 4 = do
        let first4 = [x] ++ take (chars - 1) xs
        if not $ hasRepeatN chars first4 then
            chars + n
        else
            findMarker chars (n+1) xs
    | otherwise = -1

main = do
    readFile "input.txt" >>= \inp -> print $ findMarker 4 0 inp
    readFile "input.txt" >>= \inp -> print $ findMarker 14 0 inp
