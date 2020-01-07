{-# LANGUAGE OverloadedStrings #-}

module D9_2019 where

import Intcode

main :: IO ()
main = do
  run <- runIntcode <$> progFromFile "D9/input.txt"
  putStr "Part 1: "
  print $ run [1]
  putStr "Part 2: "
  print $ run [2]
