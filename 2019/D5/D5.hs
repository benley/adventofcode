{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Data.Text as T (strip, pack, unpack, splitOn)

intcode :: Int -> [Int] -> IO (Either String [Int])

intcode _ [] = return (Right [])

intcode pos xs = let opcode = xs !! pos in
  case opcode of

    -- opcode 1: add a b dest
    1 -> let (a:b:dest:_) = drop (pos+1) xs in
      intcode (pos+4) (update dest (xs!!a + xs!!b) xs)

    -- opcode 2: multiply a b dest
    2 -> let (a:b:dest:_) = drop (pos+1) xs in
      intcode (pos+4) (update dest (xs!!a * xs!!b) xs)

    -- opcode 3: read a value, store @ dest
    3 -> do
      let dest = xs !! (pos+1)
      input <- read <$> getLine
      intcode (pos+2) (update dest input xs)

    -- opcode 4: output value@addr
    4 -> do
      let readAddr = xs !! (pos+1)
      print (xs !! readAddr)
      intcode (pos+2) xs

    -- opcode 99: halt
    99 -> return (Right xs)

    -- unknown opcode?
    x -> return (Left $ "Unknown opcode: " ++ show x)


-- | Update a list (xs) by storing value newX at index n
-- | This is not efficient, but it's good enough for now
update :: Int -> a -> [a] -> [a]
update n newX xs = take n xs ++ [newX] ++ drop (n + 1) xs

main :: IO ()
main = do
  input' <- splitOn "," . strip . pack <$> readFile "D5/input.txt"
  let input = map (read . unpack) input'
  result <- intcode 0 input
  print result
