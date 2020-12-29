{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Aoc2019.Intcode

main :: IO ()
main = do
  run <- runIntcode <$> progFromFile "Aoc2019/D5/input.txt"

  putStr "Part 1: "
  print (run [1])

  putStr "Part 2: "
  print (run [5])
