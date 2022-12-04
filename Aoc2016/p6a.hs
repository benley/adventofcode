module P6a where

import Data.List (group, sort, sortOn)

lettercount :: String -> [(Int, Char)]
lettercount = map classify . group . sort . filter (/= '-')
  where classify xs = (length xs, head xs)

mostcommon xs  = snd . last $ sortOn fst xs

leastcommon xs = snd . head $ sortOn fst xs

main = do
    indata <- lines <$> readFile "p6-input.txt"

    let columns = length $ head indata
        justcol n = [x !! n | x <- indata]
        derp fn = [(fn . lettercount) (justcol n) | n <- [0..columns-1]]

    putStrLn $ "Part 1: " ++ derp mostcommon
    putStrLn $ "Part 2: " ++ derp leastcommon
