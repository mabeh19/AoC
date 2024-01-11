module Main where
import Data.List
import Debug.Trace


data Point = Point {
    pX :: Int,
    pY :: Int
} deriving (Show, Eq)

data State = State {
    sKnots :: [Point],
    sVisited :: [Point]
} deriving (Show)

instance Num Point where
    Point a b + Point c d = Point (a + c) (b + d)
    Point a b * Point c d = Point (a * c) (b * d)
    Point a b - Point c d = Point (a - c) (b - d)
    abs (Point a b) = Point (abs a) (abs b)
    signum (Point a b) = Point (signum a) (signum b)
    fromInteger i = Point (fromInteger i) (fromInteger i)

mDist :: Point -> Point -> Int
mDist p1 p2 = max (abs (pX p1 - pX p2)) (abs (pY p1 - pY p2)) - 1

limit :: Int -> Int
limit = min 1 . max (-1)

getMovementVec :: Point -> Point -> Point
getMovementVec h t = do
    let diff = h - t
    Point (limit $ pX diff) (limit $ pY diff)

updateTail :: Point -> Point -> Point
updateTail h t
    | mDist h t < 1 = t
    | otherwise = t + getMovementVec h t

right   = Point 1 0
left    = Point (-1) 0
up      = Point 0 1
down    = Point 0 (-1)

updateHead :: Point -> Char -> Point
updateHead p 'R' = p + right
updateHead p 'L' = p + left
updateHead p 'U' = p + up
updateHead p 'D' = p + down
updateHead p _ = p

updateStates :: State -> (Char, Int) -> State
updateStates s (c, n) = foldl (\s c -> do
    let nh = updateHead (head $ sKnots s) c
    let nts = updateTails nh (tail $ sKnots s)
    State ([nh] ++ nts) (sVisited s ++ [last nts])) s $ take n $ repeat c
    where   updateTails h (x:xs) = do 
                let nt = updateTail h x
                [nt] ++ updateTails nt xs
            updateTails _ [] = []

main = do
    readFile "input.txt" >>= \inp -> print $ length $ nub $ sVisited $ foldl updateStates (State (take 2 $ repeat (Point 0 0)) []) $ map (\s -> (s !! 0, read $ drop 2 s)) $ lines inp
    readFile "input.txt" >>= \inp -> print $ length $ nub $ sVisited $ foldl updateStates (State (take 10 $ repeat (Point 0 0)) []) $ map (\s -> (s !! 0, read $ drop 2 s)) $ lines inp
