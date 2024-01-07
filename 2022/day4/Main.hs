module Main where
import Data.List
import Data.Maybe

data Section = Section {
    sectStart   :: Int,
    sectEnd     :: Int
} deriving(Show)


sectFullyContained :: Section -> Section -> Bool
sectFullyContained s1 s2 = ((sectStart s1 >= sectStart s2) && (sectEnd s1 <= sectEnd s2)) || ((sectStart s2 >= sectStart s1) && (sectEnd s2 <= sectEnd s1))

sectOverlap :: Section -> Section -> Bool
sectOverlap s1 s2 = ((sectStart s1 >= sectStart s2) && (sectStart s1 <= sectEnd s2)) || ((sectStart s2 >= sectStart s1) && (sectStart s2 <= sectEnd s1))

parseSect :: String -> Section
parseSect s = do
    let (start, end) = splitAt (fromJust $ elemIndex '-' s) s
    Section (read start) (read (drop 1 end))

checkAss :: String -> Bool
checkAss s = do
    let (first, second) = splitAt (fromJust $ elemIndex ',' s) s
    sectFullyContained (parseSect first) (parseSect (drop 1 second))

checkAss2 :: String -> Bool
checkAss2 s = do
    let (first, second) = splitAt (fromJust $ elemIndex ',' s) s
    sectOverlap (parseSect first) (parseSect (drop 1 second))

main = do
    readFile "input.txt" >>= (\inp -> print $ sum $ map (\a -> if a then 1 else 0) $ map checkAss $ lines inp)
    readFile "input.txt" >>= (\inp -> print $ sum $ map (\a -> if a then 1 else 0) $ map checkAss2 $ lines inp)
