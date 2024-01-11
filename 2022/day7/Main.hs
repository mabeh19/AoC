module Main where
import Data.List
import Data.Maybe

data FileType = Binary | Directory  deriving (Show, Eq)

data File = File {
    fType :: FileType,
    fSize :: Int,
    fName :: String
} deriving Show

data FileSystem = FileSystem {
    fsCurrentDir    :: String,
    fsFiles         :: [File]
} deriving Show

data Command = Command {
    cmdName :: String,
    cmdArg :: [String]
}

parseFile :: FileSystem -> String -> File
parseFile fs s = do
    let (first, second) = splitAt (fromJust $ elemIndex ' ' s) s
    let t = if first == "dir" then Directory else Binary
    let size = if t == Binary then read first else 0
    let name = fsCurrentDir fs ++ (drop 1 second)
    File t size name

cd :: FileSystem -> String -> FileSystem
cd fs ".." = FileSystem (reverse $ dropWhile (/= '/') $ drop 1 $ reverse $ fsCurrentDir fs) (fsFiles fs)
cd fs "/" = FileSystem "/" (fsFiles fs)
cd fs s = FileSystem (fsCurrentDir fs ++ s ++ "/") (fsFiles fs)

ls :: FileSystem -> [String] -> FileSystem
ls fs xs = FileSystem (fsCurrentDir fs) (fsFiles fs ++ map (parseFile fs) xs)

doCmd :: FileSystem -> Command -> FileSystem
doCmd fs cmd
    | cmdName cmd == "cd" = cd fs $ (cmdArg cmd) !! 0
    | cmdName cmd == "ls" = ls fs $ cmdArg cmd

parseInput :: [String] -> [Command]
parseInput [] = []
parseInput (x:xs) = do
    if x !! 0 == '$' then
        case x !! 2 of
            'c' -> [Command "cd" $ [drop 5 x]] ++ parseInput xs
            'l' -> do
                let files = takeWhile (\a -> a !! 0 /= '$') xs
                [Command "ls" files] ++ parseInput (drop (length files) xs)
    else
        []

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (s1:s1s) (s2:s2s) = s1 == s2 && startsWith s1s s2s

dirSize :: FileSystem -> String -> Int
dirSize fs p = do
    let subfiles = filter (startsWith p . fName) $ filter (\f -> fType f == Binary) $ fsFiles fs
    sum $ map fSize subfiles

getDirs :: FileSystem -> [File]
getDirs = filter (\f -> fType f == Directory) . fsFiles

totalDiskSpace = 70000000
requiredSpace = 30000000

solve1 :: String -> Int
solve1 s = do
    let fs = FileSystem "/" []
    let nfs = foldl (\fs cmd -> doCmd fs cmd) fs $ parseInput $ lines s
    sum $ filter (<= 100000) $ map (dirSize nfs . fName) $ getDirs nfs

solve2Candidates :: String -> [Int]
solve2Candidates s = do
    let fs = FileSystem "/" []
    let nfs = foldl (\fs cmd -> doCmd fs cmd) fs $ parseInput $ lines s
    let totalSize = dirSize nfs "/"
    let remSpace = totalDiskSpace - totalSize
    let missing = requiredSpace - remSpace
    sort $ filter (>= missing) $ map (dirSize nfs . fName) $ getDirs nfs

solve2 :: String -> Int
solve2 s = (take 1 $ solve2Candidates s) !! 0


main = do
    readFile "input.txt" >>= \inp -> print $ solve1 inp
    readFile "input.txt" >>= \inp -> print $ solve2 inp
