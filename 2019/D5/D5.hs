{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Data.Text as T (strip, pack, unpack, splitOn)

type Address = Int
data Arg = Value Int | Ptr Address deriving Show

data Instruction = Add Arg Arg Address
                 | Multiply Arg Arg Address
                 | Store Address
                 | Output Address
                 | Halt
                 deriving Show

-- later: generalize instruction decoding with this
--
-- nthDigit :: Int -> Int -> Int
-- nthDigit nth num = num `div` (10 ^ (nth-1)) `mod` 10
--
-- decodeN :: Int -> [Int] -> [Arg]
-- decodeN n (i:xs) =

decodeInstruction :: [Int] -> Maybe Instruction
decodeInstruction [] = Nothing
decodeInstruction (i:args) = do
  let opcode = i `mod` 100 -- rightmost two digits
  case opcode of
    1 -> do
      let mode1 = i `div` 100 `mod` 10
          mode2 = i `div` 1000 `mod` 10
          [raw1, raw2, dest] = take 3 args
          arg1 = if mode1 == 0 then Ptr raw1 else Value raw1
          arg2 = if mode2 == 0 then Ptr raw2 else Value raw2
      Just $ Add arg1 arg2 dest
    2 -> do
      let mode1 = i `div` 100 `mod` 10
          mode2 = i `div` 1000 `mod` 10
          [raw1, raw2, dest] = take 3 args
          arg1 = if mode1 == 0 then Ptr raw1 else Value raw1
          arg2 = if mode2 == 0 then Ptr raw2 else Value raw2
      Just $ Multiply arg1 arg2 dest
    3 -> Just $ Store (head args)
    4 -> Just $ Output (head args)
    99 -> Just Halt
    _ -> Nothing

intcode :: Int -> [Int] -> IO (Either String [Int])
intcode _ [] = return (Right [])
intcode pos xs = do
  let getValue (Value n) = n
      getValue (Ptr p) = xs !! p

  case decodeInstruction (drop pos xs) of

    Nothing -> return $ Left ("Could not decode opcode: " ++ show (xs !! pos))

    Just (Add arg1 arg2 dest) ->
      intcode (pos+4) (update dest (getValue arg1 + getValue arg2) xs)

    Just (Multiply arg1 arg2 dest) ->
      intcode (pos+4) (update dest (getValue arg1 * getValue arg2) xs)

    Just (Store dest) -> do
      putStr "Input: "
      input <- read <$> getLine
      intcode (pos+2) (update dest input xs)

    Just (Output addr) -> do
      print (xs !! addr)
      intcode (pos+2) xs

    Just Halt ->
      return (Right xs)

-- | Update list xs by storing value newX at index n
-- | This is not efficient, but it's good enough for now
update :: Int -> a -> [a] -> [a]
update n newX xs = take n xs ++ [newX] ++ drop (n + 1) xs

main :: IO ()
main = do
  input' <- splitOn "," . strip . pack <$> readFile "D5/input.txt"
  let input = map (read . unpack) input'
  result <- intcode 0 input
  case result of
    Left err -> putStrLn err
    Right st -> putStrLn ("Final state: " ++ show st)
