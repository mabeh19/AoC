module Main where

import Data.List (elemIndex, nubBy)
import Data.Maybe

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

data Direction = U | R | D | L deriving (Show, Eq)

data Map = Map {
        mmap    :: [String]
    ,   visited :: [(Point, Direction)]
    ,   point   :: Point
    ,   start   :: Point
    ,   dir     :: Direction
} deriving (Show)

up :: Point -> Point
up      = (+) (Point (-1)  0)

right :: Point -> Point
right   = (+) (Point   0   1)

down :: Point -> Point
down    = (+) (Point   1   0)

left :: Point -> Point
left    = (+) (Point   0 (-1))

parse :: String -> Map
parse s = do
    let m = lines s
        startPoint = findStartPoint m

    Map (lines s) [] startPoint startPoint U

findStartPoint :: [[Char]] -> Point
findStartPoint m = do
    let r = length $ takeWhile (notElem '^') m
        c = fromJust $ '^' `elemIndex` (m !! r)

    Point r c

walk :: (Map -> Bool) -> Map -> Map
walk stop m =
    if stop m then
        m
    else
        walk stop $
        if isObstacle (mmap m) (nextPoint m) then
            turn m
        else
            m { visited = (point m, dir m) : visited m, point = nextPoint m }

oob :: Map -> Bool
oob m = do
    let Point r c = point m
        (w, h) = (length $ mmap m, length $ head $ mmap m)
    c < 0 || c >= h || r < 0 || r >= w

isObstacle :: [String] -> Point -> Bool
isObstacle m p =
    not (oob $ Map m [] p p U) && (do
        let Point r c = p
        '#' == (m !! r) !! c)

nextPoint :: Map -> Point
nextPoint m = getDir (dir m) $ point m

turn :: Map -> Map
turn m = m { dir = nextDir $ dir m }

nextDir :: Direction -> Direction
nextDir U = R
nextDir R = D
nextDir D = L
nextDir L = U

getDir :: Direction -> Point -> Point
getDir U = up
getDir R = right
getDir D = down
getDir L = left

solve1 :: String -> IO ()
solve1 = print . length . nubBy pointsEq . visited . walk oob . parse

walk2 :: Map -> (Point, b) -> Bool
walk2 m (p, _) = do
    let sp = start m
        withObstacle = Map (replace (mmap m) p '#') [] sp sp U
    loops $ walk (\m -> loops m || oob m) withObstacle

replace :: [[a]] -> Point -> a -> [[a]]
replace m p c = do
    let before = take (row p) m
        after = drop (row p + 1) m
        mid     = m !! row p
        middle = [take (col p) mid ++ [c] ++ drop (col p + 1) mid]
    before ++ middle ++ after

loops :: Map -> Bool
loops m = do
    let current = (point m, dir m)
    current `elem` visited m

getObstacles :: Map -> [(Point, Direction)]
getObstacles m = filter (walk2 m) $ nubBy pointsEq $ visited m

pointsEq (a, _) (b, _) = a == b

solve2 :: String -> IO ()
solve2 = print . length . getObstacles . walk oob . parse

main :: IO ()
main = do
    readFile "test.txt" >>= \f -> do
        solve1 f
        solve2 f
    readFile "input.txt" >>= \f -> do
        solve1 f
        solve2 f
