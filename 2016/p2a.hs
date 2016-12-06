module P2 where

import Data.String.Utils (strip)

pinpad = [[1,2,3],
          [4,5,6],
          [7,8,9]]

button (x, y) = pinpad !! y !! x

move :: (Int, Int) -> Char -> (Int, Int)
move (x0, y0) dir =
    if x0 < xmin || y0 < ymin || x0 > xmax || y0 > ymax
      then error "off the grid. wtf?"
      else case dir of
        'U' -> if y0 == ymax then (x0, y0) else (x0, y0 - 1)
        'D' -> if y0 == ymin then (x0, y0) else (x0, y0 + 1)
        'R' -> if x0 == xmax then (x0, y0) else (x0 + 1, y0)
        'L' -> if x0 == xmin then (x0, y0) else (x0 - 1, y0)
    where xmin = 0
          ymin = 0
          xmax = length (head pinpad) - 1
          ymax = length pinpad - 1

runlines :: (Int, Int) -> [String] -> [(Int, Int)]
runlines _ [] = []
runlines pos (move_seq:moves) =
    newpos : runlines newpos moves
    where newpos = foldl move pos move_seq

main :: IO ()
main = do
    cmds <- lines <$> readFile "p2-input.txt"
    putStrLn $ concatMap (show . button) $ runlines (1,1) cmds
