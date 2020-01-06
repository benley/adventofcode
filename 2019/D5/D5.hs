{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Intcode

main :: IO ()
main = do
  prog <- progFromFile "D5/input.txt"

  putStr "Part 1: "
  print (intcode [1] 0 prog)

  putStr "Part 2: "
  print (intcode [5] 0 prog)
