module Main where

import Data.List

data Point = Point {
        row :: Int
    ,   col :: Int
} deriving (Show, Eq)

instance Num Point where
    Point a b + Point c d = Point (a+c) (b+d)
    Point a b * Point c d = Point (a*c) (b*d)
    Point a b - Point c d = Point (a-c) (b-d)
    abs    (Point a b) = Point (abs a)  (abs b)
    signum (Point a b) = Point (signum a) (signum b)
    fromInteger i = Point (fromInteger i) (fromInteger i)

(.!!) :: [[a]] -> Point -> a
(.!!) m p = (m !! row p) !! col p

oob :: Foldable t => [t a] -> Point -> Bool
oob m p = do
    let Point r c = p
        (w, h) = (length m, length $ head m)
    c < 0 || c >= h || r < 0 || r >= w


data Day8 = Day8 {
        _map        :: [String]
    ,   antinodes   :: [Point]
    ,   antennae    :: [(Char, [Point])]
} deriving (Show)


parse :: String -> Day8
parse s = Day8 (lines s) [] $ generateAntennae $ lines s

generateAntennae :: [[Char]] -> [(Char, [Point])]
generateAntennae m = do
    let len = length m - 1
        idxs = concatMap (map (uncurry Point) . zip [0..len] . repeat) [0..len]
    foldl (tryInsertAntenna m) [] idxs

tryInsertAntenna :: [[Char]] -> [(Char, [Point])] -> Point -> [(Char, [Point])]
tryInsertAntenna m as p = do
    let c = m .!! p

    case c of 
        '.' -> as
        freq   -> appendAntenna as freq p

appendAntenna :: [(Char, [Point])] -> Char -> Point -> [(Char, [Point])]
appendAntenna [] c p = [(c, [p])]
appendAntenna ((c1, ps):as) c2 p
    | c1 == c2 = (c1, p : ps) : as
    | otherwise = (c1, ps) : appendAntenna as c2 p

calculateNodes :: Day8 -> Day8
calculateNodes state = foldl findAntinodesForFrequency state $ antennae state

findAntinodesForFrequency :: Day8 -> (Char, [Point]) -> Day8
findAntinodesForFrequency state (_, points) = do 
    let m = _map state 
        as = concatMap (findAntinodesForAntenna (_map state) points) points
    state { antinodes = antinodes state ++ as }

findAntinodesForAntenna :: [String] -> [Point] -> Point -> [Point]
findAntinodesForAntenna m as p = concatMap (nodesForPair m p) as

nodesForPair :: [String] -> Point -> Point -> [Point]
nodesForPair m p1 p2 
    | p1 == p2 = []
    | otherwise = do
        let dif = p1 - p2
        takeValid (scanl (+) p1 (repeat dif)) ++ takeValid (scanl (-) p2 (repeat dif))

    where takeValid = takeWhile (isInsideMap m)

antinodesInsideMap :: Day8 -> [Point]
antinodesInsideMap state = filter (isInsideMap $ _map state) $ antinodes state

isInsideMap :: Foldable t => [t a] -> Point -> Bool
isInsideMap m p = not $ oob m p


solve1 :: String -> IO ()
solve1 = print . length . nub . antinodesInsideMap . calculateNodes . parse 

main :: IO ()
main = do
    readFile "test.txt" >>= solve1
    readFile "input.txt" >>= solve1
