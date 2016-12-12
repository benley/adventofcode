module P3b where

isValidTri :: Integral a => [a] -> Bool
isValidTri [a, b, c] = a + b > c && a + c > b && b + c > a

vertBarf :: Integral a => [[a]] -> [[a]]
vertBarf [] = []
vertBarf ([a1, a2, a3] : [b1, b2, b3] : [c1, c2, c3] : xs) =
    [a1, b1, c1] : [a2, b2, c2] : [a3, b3, c3] : vertBarf xs

main = do
    indata <- lines <$> readFile "p3-input.txt"
    let readWords x = map read (words x)
        tris = vertBarf $ map readWords indata
    print (length (filter (==True) (map isValidTri tris)))
