{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Data.Text as T (strip, pack, unpack, splitOn)

type Address = Int
data Arg = Value Int | Ptr Address deriving Show

data Instruction = Add Arg Arg Address
                 | Multiply Arg Arg Address
                 | Store Address
                 | Output Arg
                 | JumpIfTrue Arg Arg
                 | JumpIfFalse Arg Arg
                 | LessThan Arg Arg Address
                 | Equal Arg Arg Address
                 | Halt
                 deriving Show

nthDigit :: Int -> Int -> Int
nthDigit nth num = num `div` (10 ^ (nth-1)) `mod` 10

argN :: Int -> [Int] -> Arg
argN n xs@(i:_) = do
  let mode = nthDigit (n+2) i
  modeArg mode (xs !! n)

modeArg :: Int -> Int -> Arg
modeArg 0 v = Ptr v
modeArg 1 v = Value v
modeArg _ _ = error "invalid mode"

decodeInstruction :: [Int] -> Maybe Instruction
decodeInstruction [] = Nothing
decodeInstruction ia@(i:args) = do
  let opcode = i `mod` 100 -- rightmost two digits
  case opcode of
    1 -> Just $ Add         (argN 1 ia) (argN 2 ia) (ia !! 3)
    2 -> Just $ Multiply    (argN 1 ia) (argN 2 ia) (ia !! 3)
    3 -> Just $ Store       (head args)
    4 -> Just $ Output      (argN 1 ia)
    5 -> Just $ JumpIfTrue  (argN 1 ia) (argN 2 ia)
    6 -> Just $ JumpIfFalse (argN 1 ia) (argN 2 ia)
    7 -> Just $ LessThan    (argN 1 ia) (argN 2 ia) (ia !! 3)
    8 -> Just $ Equal       (argN 1 ia) (argN 2 ia) (ia !! 3)
    99 -> Just Halt
    _ -> Nothing

intcode :: Int -> [Int] -> IO (Either String [Int])
intcode _ [] = return (Left "Unexpected end of program")
intcode pos xs = do
  let getValue (Value n) = n
      getValue (Ptr p) = xs !! p
      instruction = decodeInstruction (drop pos xs)

  print instruction
  case instruction of

    Nothing -> return $ Left ("Could not decode opcode: " ++ show (xs !! pos))

    Just (Add arg1 arg2 dest) ->
      intcode (pos+4) (update dest (getValue arg1 + getValue arg2) xs)

    Just (Multiply arg1 arg2 dest) ->
      intcode (pos+4) (update dest (getValue arg1 * getValue arg2) xs)

    Just (Store dest) -> do
      putStr "Input: "
      input <- read <$> getLine
      intcode (pos+2) (update dest input xs)

    Just (Output arg) -> do
      print (getValue arg)
      intcode (pos+2) xs

    Just (JumpIfTrue arg target) ->
      if getValue arg /= 0
      then intcode (getValue target) xs
      else intcode (pos+2) xs

    Just (JumpIfFalse arg target) ->
      if getValue arg == 0
      then intcode (getValue target) xs
      else intcode (pos+2) xs

    Just (LessThan arg1 arg2 dest) -> do
      let v = if getValue arg1 < getValue arg2 then 1 else 0
      intcode (pos+4) (update dest v xs)

    Just (Equal arg1 arg2 dest) -> do
      let v = if getValue arg1 == getValue arg2 then 1 else 0
      intcode (pos+4) (update dest v xs)

    Just Halt -> return (Right xs)

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
