module Main where

data Action = Rock | Paper | Scissors deriving (Enum)
data Result = Loss | Draw | Win deriving (Enum)

resultScores = [0, 3, 6]
actionResults = [
    [Draw, Loss, Win],
    [Win, Draw, Loss],
    [Loss, Win, Draw]]
result2Action = [
    [Scissors, Rock, Paper],
    [Rock, Paper, Scissors],
    [Paper, Scissors, Rock]]

getAction :: Char -> Action
getAction s
    | s == 'A' || s == 'X' = Rock
    | s == 'B' || s == 'Y' = Paper
    | s == 'C' || s == 'Z' = Scissors

getOutcome :: Char -> Result
getOutcome 'X' = Loss
getOutcome 'Y' = Draw
getOutcome 'Z' = Win

getScore :: Action -> Result -> Int
getScore a r = fromEnum a + 1 + resultScores !! fromEnum r

getResult :: Action -> Action -> Result
getResult a1 a2 = (actionResults !! (fromEnum a1)) !! fromEnum a2

simGame :: String -> Int
simGame s = getScore (getAction (s !! 2)) $ getResult (getAction (s !! 2)) (getAction (s !! 0))

simGame2 :: String -> Int
simGame2 s = do
    let p_act = (result2Action !! (fromEnum $ getAction (s !! 0))) !! (fromEnum $ getOutcome (s !! 2))
    getScore p_act $ getOutcome (s !! 2)

main = do
    readFile "input.txt" >>= (\inp -> print $ sum $ map simGame $ lines inp)
    readFile "input.txt" >>= (\inp -> print $ sum $ map simGame2 $ lines inp)
