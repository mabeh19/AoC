module Main where
import Data.List

type Forest = [[Int]]

isVisibleInRow :: [Int] -> Bool
isVisibleInRow [] = True
isVisibleInRow (x:xs) = isTaller x xs
    where   isTaller x (y:xs) = x > y && isTaller x xs
            isTaller x [] = True

tryTakeOne :: [a] -> [a]
tryTakeOne [] = []
tryTakeOne xs = take 1 xs

scenicScoreRow :: [Int] -> Int
scenicScoreRow (x:[]) = 0
scenicScoreRow (x:xs) = do
    let smaller = length $ takeWhile (< x) xs
    let extra = length $ tryTakeOne $ drop smaller xs
    smaller + extra

scenicScore :: Forest -> Int -> Int -> Int
scenicScore f x y = do
    let l_r = reverse $ take (x + 1) $ f !! y
    let r_r = drop x $ f !! y
    let u_c = reverse $ take (y + 1) $ map (!! x) f
    let d_c = drop y $ map (!! x) f
    (scenicScoreRow l_r) * (scenicScoreRow r_r) * (scenicScoreRow u_c) * (scenicScoreRow d_c)


isVisible :: Forest -> Int -> Int -> Bool
isVisible f x y = do
    let l_r = reverse $ take (x + 1) $ f !! y
    let r_r = drop x $ f !! y
    let u_c = reverse $ take (y + 1) $ map (!! x) f
    let d_c = drop y $ map (!! x) f
    (isVisibleInRow l_r) || (isVisibleInRow r_r) || (isVisibleInRow u_c) || (isVisibleInRow d_c)


parseForest :: String -> Forest
parseForest = map (map (\c -> fromEnum c - 0x30)) . lines

main = do
    tst_inp <- readFile "input.txt" 
    let forest = parseForest tst_inp
    let rows = length forest
    let cols = length $ forest !! 0
    print $ length $ filter (\(x, y) -> isVisible forest x y) $ concat $ map (zip [0 .. cols - 1] . repeat) [0 .. rows - 1]
    print $ head $ reverse $ sort $ map (\(x, y) -> scenicScore forest x y) $ concat $ map (zip [0 .. cols - 1] . repeat) [0 .. rows - 1]
