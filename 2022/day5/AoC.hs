module AoC where

--split :: Char -> String -> [String]
--split c s = do
--    if (length s) > 0 then do
--        let substr = takeWhile (/= c) s
--        let rem = drop (1 + length substr) s
--        [substr] ++ split c rem
--    else
--        []

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f l = do
    let suba = takeWhile (\a -> not $ f a) l
    let rem = drop (1 + length suba) l
    [suba] ++ split f rem
