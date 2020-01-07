{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Intcode

main :: IO ()
main = do
  run <- runIntcode <$> progFromFile "D5/input.txt"

  putStr "Part 1: "
  print (run [1])

  putStr "Part 2: "
  print (run [5])
