import Data.Char
import Data.List
import Debug.Trace

incString :: String -> String
incString s = do
    let rs = reverse s
    reverse $ incChars 1 $ case find (\c -> not (isNotConfusing c)) rs of
                Just _  -> moveToNext rs
                _       -> rs

incChars :: Int -> String -> String
incChars inc (c:cs) = do 
    let (newChar, carry) = getNext inc c
    [newChar] ++ incChars carry cs
incChars c [] = ""

moveToNext :: String -> String 
moveToNext str = do
    let top = takeWhile (\c -> (isNotConfusing c)) $ reverse str
    let repl = length str - length top - 1
    reverse $ top ++ [getNextChar ((dropWhile (\c -> (isNotConfusing c)) $ reverse str) !! 0)] ++ (take repl $ repeat 'a')

getNextChar :: Char -> Char
getNextChar c = fst $ getNext 1 c

getNext :: Int -> Char -> (Char, Int)
getNext 1 'z' = ('a', 1)
getNext 1 c = (chr (ord c + 1), 0)
getNext 0 c = (c, 0)


isNotConfusing :: Char -> Bool
isNotConfusing 'i' = False
isNotConfusing 'o' = False
isNotConfusing 'l' = False
isNotConfusing _   = True

recurse :: (String -> Bool) -> String -> String
recurse isTrue s = if not (isTrue s) then recurse isTrue $ incString s else s

isCorrect :: String -> Bool
isCorrect s = (includesStraightOfThree s) && (includesNoConfusingLetters s) && (nonOverlappingPairs 0 s >= 2)


includesStraightOfThree :: String -> Bool
includesStraightOfThree (c1:c2:c3:cs) = ((1 + ord c1) == (ord c2) && (1 + ord c2) == (ord c3)) || includesStraightOfThree ([c2, c3] ++ cs)
includesStraightOfThree (c1:c2:[]) = False


includesNoConfusingLetters :: String -> Bool
includesNoConfusingLetters (c:cs) = isNotConfusing c && includesNoConfusingLetters cs
includesNoConfusingLetters [] = True

nonOverlappingPairs :: Int -> String -> Int
nonOverlappingPairs x (c1:c2:cs) = if c1 == c2 then nonOverlappingPairs (x+1) cs else nonOverlappingPairs x ([c2] ++ cs)
nonOverlappingPairs x (c1:cs) = x
nonOverlappingPairs x [] = x

main = do
    let l = "cqjxjnds"
    let first = recurse isCorrect l
    putStrLn first
    putStrLn $ recurse isCorrect $ incString first

