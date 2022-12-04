module Main where

import Data.String.Utils (split, strip)

data Direction = North | East | South | West

runbunny :: Integral a => Read a => (a, a) -> Direction -> [String] -> (a, a)
runbunny (x, y) _ [] = (x, y)
runbunny (x, y) facing ((dir:distance):xs) =
    runbunny newpos newdir xs
    where
      newpos = case newdir of
                 North -> (x, y + dist')
                 East -> (x + dist', y)
                 South -> (x, y - dist')
                 West -> (x - dist', y)
      dist' = read distance
      newdir = case dir of
                 'R' -> case facing of
                          North -> East
                          East -> South
                          South -> West
                          West -> North
                 'L' -> case facing of
                          North -> West
                          West -> South
                          South -> East
                          East -> North
                 _   -> error "invalid"

main :: IO ()
main = do
    input <- map strip . split "," . strip <$> readFile "D1/input.txt"
    let (x, y) = runbunny (0,0) North input
    print ((abs x + abs y) :: Integer)
