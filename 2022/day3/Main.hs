module Main where
import Data.Char
import Data.List

shared :: String -> String -> Char
shared (s1:s1s) s2 = if elem s1 s2 then s1 else shared s1s s2

sharedL :: String -> [String] -> Char
sharedL (s1:s1s) ss = if isShared s1 ss then s1 else sharedL s1s ss
    where   isShared c (x:xs) = elem c x && isShared c xs
            isShared _ [] = True

convert :: Char -> Int
convert c
    | isLower c = 1 + fromEnum c - fromEnum 'a'
    | isUpper c = 1 + 26 + fromEnum c - fromEnum 'A'

checkRucksack :: String -> Int
checkRucksack s = do
    let (l, r) = splitAt (length s `div` 2) s
    convert $ shared l r

getBadge :: [String] -> Int
getBadge (x:xs) = convert $ sharedL x xs

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = 
    let (ys, zs) = splitAt n xs
    in ys : chunks n zs 

main = do
    readFile "input.txt" >>= (\inp -> print $ sum $ map checkRucksack $ lines inp)
    readFile "input.txt" >>= (\inp -> print $ sum $ map getBadge $ chunks 3 $ lines inp)
