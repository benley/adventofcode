module Main where

import Data.Map.Lazy
import Data.Text
import Text.Printf

runSanta :: String -> (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
runSanta [] _ houses = houses
runSanta (v:v_rest) (x, y) houses =
    runSanta v_rest newPos (insert newPos 1 houses)
    where newPos = case v of '<' -> (x-1, y)
                             '>' -> (x+1, y)
                             '^' -> (x, y+1)
                             'v' -> (x, y-1)
                             _ -> error "wat"

oneSanta :: String -> Map (Int, Int) Int
oneSanta input = runSanta input (0, 0) (fromList [((0,0), 1)])

deinterlace :: String -> (String, String)
deinterlace str = deint str [] [] where
  deint :: String -> String -> String -> (String, String)
  deint [] xs ys = (Prelude.reverse xs, Prelude.reverse ys)
  deint [x] xs ys = deint [] (x:xs) ys
  deint [x, y] xs ys = deint [] (x:xs) (y:ys)
  deint (x:y:zs) xs ys = deint zs (x:xs) (y:ys)

alternateSantas :: String -> Int
alternateSantas input = size $ union (oneSanta s1houses) (oneSanta s2houses) where
  (s1houses, s2houses) = deinterlace input

main :: IO ()
main = do
    input <- unpack . strip . pack <$> readFile "D3/input.txt"
    printf "Part 1: %d\n" $ size $ oneSanta input
    printf "Part 2: %d\n" $ alternateSantas input
