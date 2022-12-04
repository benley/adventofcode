#!/usr/bin/env runhaskell

module D3 where

import Data.Map as M
-- import Data.Array

main :: IO ()
main = do
  print (part1 368078)
  print ""

-- signs = repeat [(+), (-)]

-- foo2 n (x, y) f =
--   take n (zip [f x 1, (f x 2) ..] (repeat y))
--   : take n (zip (repeat (f x n)) [f y 1, (f y 2) ..])
--   : foo2 (n+1) (f x n, f y n) nextF
--   where nextF =
--           case f of
--             (+) -> (-)
--             (-) -> (+)



foo :: Int -> (Int, Int) -> Bool -> [[(Int, Int)]]
foo n (x, y) False =
  take n (zip [x+1, x+2 ..] (repeat y))
  : take n (zip (repeat (x+n)) [y+1, y+2 ..])
  : foo (n+1) (x+n, y+n) True

foo n (x, y) True =
  take n (zip [x-1, x-2 ..] (repeat y))
  : take n (zip (repeat (x-n)) [y-1, y-2 ..])
  : foo (n+1) (x-n, y-n) False

theFuckingSequence :: [(Int, Int)]
theFuckingSequence = concat $ [(0,0)] : foo 1 (0,0) False

part1 :: Int -> Int
part1 n = let (x, y) = theFuckingSequence !! (n-1) in (abs x) + (abs y)


squareGrid 1 = M.fromList [(1,1)]
squareGrid n = M.insert nextPos nextVal lastGrid where
  lastGrid = squareGrid (n-1)
  nextPos = undefined
  nextVal = sumOfNeighbors nextPos lastGrid

data Grid = Map Int Int
data Position = (Int,Int)

sumOfNeighbors :: Position -> Grid -> Int
sumOfNeighbors pos grid =
