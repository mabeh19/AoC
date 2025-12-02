module Main where

import qualified Control.Monad.State.Lazy as S
import qualified Data.HashMap.Strict as HM

parse :: String -> [Int]
parse = map read . words

type State = S.State (HM.HashMap Input Int)

type Input = (Int, Int)

blink :: Int -> Int -> State Int
blink 0 _ = return 1
blink n 0 = blink (n - 1) 1
blink n s = do
    his <- S.get
    case HM.lookup (s, n) his of
        Just x -> return x
        Nothing -> do
            if even (length $ show s) then do
                let as_str = show s
                    len = length as_str
                    (s1, s2) = splitAt (len `div` 2) as_str

                res1 <- blink (n - 1) (read s1)
                res2 <- blink (n - 1) (read s2)

                let res = res1 + res2

                S.modify (HM.insert (s, n) res)

                return res

            else do 
                res <- blink (n - 1) (s * 2024)
                S.modify (HM.insert (s, n) res)
                return res

getSum :: Int -> [Int] -> Int
getSum n = sum . map (\s -> S.evalState (blink n s) HM.empty)

solve1 :: String -> IO ()
solve1 = print . getSum 25 . parse

solve2 :: String -> IO ()
solve2 = print . getSum 75 . parse

main :: IO ()
main = do
    readFile "input.txt" >>= solve1
    readFile "input.txt" >>= solve2
