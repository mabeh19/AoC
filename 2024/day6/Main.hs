module Main where

import Data.Maybe
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

type Occurrence = [Point]

data State a = State {
        occurrences :: [a]
    ,   puzzle      :: [String]
    ,   needle      :: String
} deriving (Show, Eq)


dirs :: [Point]
dirs = [
        Point 1 0
    ,   Point 0 1
    ,   Point 1 1
    ,   Point (-1) 1
    ]

dirs2 :: [Point]
dirs2 = [
        Point 1    1
    ,   Point (-1) 1
    ]

parse :: String -> String -> State a
parse n s = State [] (lines s) n

getOccurrences :: State Occurrence -> State Occurrence
getOccurrences state = foldl getOccurrencesForPoint state $ generatePoints state

generatePoints :: State a -> [Point]
generatePoints state = concat $ map (map (uncurry Point)) $ map (\n -> zip (repeat n) [0..(length $ puzzle state) - 1]) $ [0..(length $ (puzzle state !! 0)) - 1]

getOccurrencesForPoint :: State Occurrence -> Point -> State Occurrence
getOccurrencesForPoint state point = do
    let maybeOccurrences = map (tryGetOccurrence state point) dirs
        newOccurrences = map fromJust $ filter isJust maybeOccurrences
    state { occurrences = occurrences state ++ newOccurrences }

tryGetOccurrence :: State a -> Point -> Point -> Maybe [Point]
tryGetOccurrence state point dir = do
    let line = createLine state point dir
        xmas = needle state
    if length line < length xmas
    then Nothing
    else if matchesNeedle line xmas || (matchesNeedle line $ reverse xmas) then
        Just line
    else Nothing

    where 
        matchesNeedle line = all (uncurry (==)) . zip (map (getCharAtPoint (puzzle state)) line)

createLine :: State a -> Point -> Point -> [Point]
createLine state point dir = do
    let p = puzzle state
        c = getCharAtPoint p point
        offset = getOffset (needle state) c
        points = map (indexForPoint point offset dir) [0..(length $ needle state) - 1]
    limitPointsToPuzzle p points

limitPointsToPuzzle :: Foldable t => [t a] -> [Point] -> [Point]
limitPointsToPuzzle p = filter (\point -> not (col point < 0 || row point < 0 || col point >= length (p !! 0) || row point >= length p))

getCharAtPoint :: [[a]] -> Point -> a
getCharAtPoint p point = (p !! row point) !! col point

getOffset :: String -> Char -> Int
getOffset xmas c = case elemIndex c xmas of 
                Just n -> n
                Nothing -> -1

indexForPoint :: Point -> Int -> Point -> Int -> Point
indexForPoint point offset dir n = point + (n - offset) .* dir

(.*) :: Int -> Point -> Point
(.*) n p = Point (n * row p) (n * col p)


getOccurrences2 :: State [[Point]] -> State [[Point]]
getOccurrences2 state = foldl getOccurrencesForPoint2 state $ generatePoints state

getOccurrencesForPoint2 :: State [[Point]] -> Point -> State [[Point]]
getOccurrencesForPoint2 state point = 
    if getCharAtPoint (puzzle state) point /= 'A' then
        state
    else do
        let maybeOccurrences = map (tryGetOccurrence state point) dirs2
            newOccurrences = map fromJust $ filter isJust maybeOccurrences

        if length newOccurrences == 2 then
            state { occurrences = occurrences state ++ [newOccurrences] }
        else 
            state

solve1 :: String -> IO ()
solve1 = print . length . nub . (\state -> (occurrences $ getOccurrences (state { needle = reverse $ needle state })) ++ (occurrences $ getOccurrences state)) . parse "XMAS"


solve2 :: String -> IO ()
solve2 = print . length . nub . occurrences . getOccurrences2 . parse "MAS"

main :: IO ()
main = do
    readFile "test.txt" >>= \f -> do 
        solve1 f
        solve2 f
    readFile "input.txt" >>= \f -> do
        solve1 f
        solve2 f
