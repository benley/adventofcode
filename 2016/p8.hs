{-# LANGUAGE ParallelListComp #-}

module P8 where

import qualified Data.List
import Data.List (foldl')
import Data.List.Split (splitOn)

data Pixel = On | Off | Null deriving Eq

pixelChar On   = '#'
pixelChar Off  = '.'
pixelChar Null = ' '

data Grid = Grid [[Pixel]]

instance Show Grid where show = showgrid

showgrid :: Grid -> String
showgrid (Grid g) = unlines [map pixelChar r | r <- g]

rect :: Int -> Int -> Grid
rect w h =
    Grid (replicate h (replicate w On ++ replicate (gridW - w) Null)
          ++ replicate (gridH - h) (replicate gridW Null))

overlay :: Grid -> Grid -> Grid
overlay (Grid topG) (Grid btmG) =
    Grid [zipWith (|.|) rowT rowB | rowT <- topG | rowB <- btmG]

-- ternary "or" or something like that
(|.|) :: Pixel -> Pixel -> Pixel
Null |.| b = b
a    |.| _ = a

rotate :: Int -> [a] -> [a]
rotate n xs = reverse $ take (length xs) (drop n (cycle (reverse xs)))

rotateRow :: Int -> Int -> Grid -> Grid
rotateRow rowN byN (Grid grid) =
    Grid (take rowN grid
          ++ [rotate byN (grid !! rowN)]
          ++ drop (rowN+1) grid)

rotateCol :: Int -> Int -> Grid -> Grid
rotateCol colN byN grid = transpose (rotateRow colN byN (transpose grid))

transpose :: Grid -> Grid
transpose (Grid g) = Grid (Data.List.transpose g)

litPx :: Grid -> Int
litPx (Grid g) = length (filter (==On) (concat g))

---- hacky parsin' time

runCmd :: [String] -> Grid -> Grid

runCmd ["rect", wXh] = overlay (rect w h)
    where [w, h] = map read (splitOn "x" wXh)

runCmd ["rotate", "row", yEqN, "by", n] = rotateRow rowN (read n)
    where rowN = read $ last (splitOn "=" yEqN)

runCmd ["rotate", "column", xEqN, "by", n] = rotateCol colN (read n)
    where colN = read $ last (splitOn "=" xEqN)

runMany :: [[String]] -> Grid -> Grid
runMany cmds grid = foldl' (flip runCmd) grid cmds

gridW = 50
gridH = 6
initRow = replicate gridW Null
initGrid = Grid (replicate gridH initRow)

main = do
    cmdlines <- map words . lines <$> readFile "p8-input.txt"
    let result = runMany cmdlines initGrid
    print result
    putStrLn $ "Lit pixels: " ++ show (litPx result)
