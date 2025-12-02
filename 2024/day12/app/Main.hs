module Main where

import Data.List
import Debug.Trace

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

type Region = [Point]

data State = State {
        _map    :: [String]
    ,   regions :: [Region]
}

parse :: String -> State
parse s = State (lines s) $ getRegions (lines s) [] (Point 0 0)

allPoints :: Foldable t => [t a] -> [Point]
allPoints m = concatMap (\r -> map (Point r) [0..length (head m) - 1]) [0..length m - 1]

getRegions :: [String] -> [[Point]] -> Point -> [[Point]]
getRegions m rs p = do
    let r = getRegion m [p]
        new_rs = r : rs
        remP = dropWhile (\np -> any (np `elem`) new_rs) $ allPoints m

    case remP of
        []      -> new_rs
        (rp:_)  -> getRegions m new_rs rp

getRegion :: [String] -> [Point] -> [Point]
getRegion m ps = do
    let plots = nub $ concatMap (filter (`notElem` ps) . adjacentPlots m) ps ++ ps

    if length plots == length ps then
        plots
    else
        getRegion m plots

fencePrice :: [String] -> [Point] -> Int
fencePrice m r = do
    let perim = perimeter m r
        a = area r

    perim * a

showRegions m = map (m .!!)

area :: [a] -> Int
area = length

perimeter :: [String] -> [Point] -> Int
perimeter _ [] = 0
perimeter m (p:ps) = let perim = length (filter (\sp -> oob m sp || m .!! p /= m .!! sp) $ sorroundingPlots m p) + perimeter m ps
                     in perim

countCorners :: [String] -> [Point] -> Int
countCorners _ [_] = 4
countCorners m r = sum $ map (\p -> 
    if isDeadend r p then
        2
    else
        enumCorners m r p) r

enumCorners :: [String] -> [Point] -> Point -> Int
enumCorners m r p = length $ filter (isCorner r) $ map (p +) dirs

isCorner :: [Point] -> Point -> Bool
isCorner r p = any (checkCorner r p) corners 

checkCorner :: [Point] -> Point -> [Point] -> Bool
checkCorner r p dirs = all (\d -> (p + d) `elem` r && (p + sum dirs) `notElem` r) dirs

corners :: [[Point]]
corners = [
        [Point   1   0, Point   0   1 ]
    ,   [Point (-1)  0, Point   0   1 ]
    ,   [Point   1   0, Point   0 (-1)]
    ,   [Point (-1)  0, Point   0 (-1)]
    ]

isDeadend :: [Point] -> Point -> Bool
isDeadend r p =
    let sps = map (p +) dirs 
    in
    1 == length (filter (`elem` r) sps)

adjacentPlots :: Eq a => [[a]] -> Point -> [Point]
adjacentPlots m p = filter (\n -> m .!! p == m .!! n) $ filter (not . oob m) $ map (p +) dirs

sorroundingPlots :: [[a]] -> Point -> [Point]
sorroundingPlots _ p = map (p +) dirs

bulkPrice m r = 
    let a = area r
        sides = countCorners m r
    in
    sides * a

dirs :: [Point]
dirs = [
        Point   1    0
    ,   Point (-1)   0
    ,   Point   0    1
    ,   Point   0  (-1)
    ]

solve1 :: String -> IO ()
solve1 s = do
    let state = parse s
    print $ sum $ map (fencePrice $ _map state) $ regions state

solve2 :: String -> IO ()
solve2 s = do
    let state = parse s
    print $ sum $ map (bulkPrice $ _map state) $ regions state

main :: IO ()
main = do
    readFile "test.txt" >>= \f -> do 
        solve1 f
        solve2 f
    readFile "input.txt" >>= \f -> do 
        solve1 f
        solve2 f
