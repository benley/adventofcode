module P3a where

import Data.String.Utils (strip)

isValidTri :: Integral a => [a] -> Bool
isValidTri [a, b, c] = a + b > c && a + c > b && b + c > a

main = do
    indata <- map words . lines <$> readFile "p3-input.txt"
    print $ length (filter (==True) (map (isValidTri . map read) indata))
