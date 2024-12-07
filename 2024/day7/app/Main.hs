module Main where


data Equation = Equation {
        result  :: Integer
    ,   input   :: [Integer]
} deriving (Show, Eq)


eqs :: [Integer -> Integer -> Integer]
eqs = [(+), (*), (.||)]

parse :: String -> [Equation]
parse = map (\l -> Equation (read $ takeWhile ((/=) ':') l) (map read $ words $ tail $ dropWhile ((/=) ':') l)) . lines

possibleEquations :: [Equation] -> [Equation]
possibleEquations = filter equationIsPossible

equationIsPossible :: Equation  -> Bool
equationIsPossible e = any (checkisPossible e) $ generateEquations e

checkisPossible :: Equation -> [Integer -> Integer] -> Bool
checkisPossible e eval = do 
    let expected = result e
        res = dropWhile (< result e) $ scanl (\t eq -> eq t) (head $ input e) eval
    case res of 
        [] -> False
        (x:_) -> expected == x

generateEquations :: Equation -> [[Integer -> Integer]]
generateEquations = sequence . map (\n -> map (\eq -> (\x -> eq x n))  eqs) . tail . input

solve :: String -> IO ()
solve = print . sum . map result . possibleEquations . parse

(.||) :: Integer -> Integer -> Integer
(.||) a b = read (show a ++ show b)


main :: IO ()
main = do
    readFile "test.txt" >>= solve
    readFile "input.txt" >>= solve
