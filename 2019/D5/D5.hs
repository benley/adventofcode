{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Data.Text as T (strip, pack, unpack, splitOn)

import Intcode

main :: IO ()
main = do
  input' <- splitOn "," . strip . pack <$> readFile "D5/input.txt"
  let input = map (read . unpack) input'
  result <- intcode 0 input
  case result of
    Left err -> putStrLn err
    Right st -> putStrLn ("Final state: " ++ show st)
