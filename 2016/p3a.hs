module P3a where

import Data.String.Utils (strip)

isValidTri :: Integral a => [a] -> Bool
isValidTri [a, b, c] = a + b > c && a + c > b && b + c > a

main = do
    indata <- lines <$> readFile "p3-input.txt"
    let tris = map words indata
    print (length $ filter (==True) (map (isValidTri . map read) tris))
