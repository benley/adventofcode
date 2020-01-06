{-# LANGUAGE OverloadedStrings #-}

module Intcode (
  intcode,
  toProgram,
  progFromFile,
  Program
) where

import Data.Array
import Data.Text (strip, unpack, splitOn)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

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

type Program = Array Int Int

-- | Convert an Int array containing an intcode program into a Program
toProgram :: [Int] -> Program
toProgram p = listArray (0, length p - 1) p

-- | Read a file, return its contents as a Program
progFromFile :: FilePath -> IO Program
progFromFile f = toProgram . map (read . unpack) . splitOn "," . strip <$> readFile f

-- | return the nth digit (from the right) of an integer, starting at 1
nthDigit :: Int -> Int -> Int
nthDigit nth num = num `div` (10 ^ (nth-1)) `mod` 10

decodeInstruction :: [Int] -> Maybe Instruction
decodeInstruction [] = Nothing
decodeInstruction ia@(i:_) =
  case opcode of
    1 -> Just $ Add         (argN 1) (argN 2) (ia !! 3)
    2 -> Just $ Multiply    (argN 1) (argN 2) (ia !! 3)
    3 -> Just $ Store       (ia !! 1)
    4 -> Just $ Output      (argN 1)
    5 -> Just $ JumpIfTrue  (argN 1) (argN 2)
    6 -> Just $ JumpIfFalse (argN 1) (argN 2)
    7 -> Just $ LessThan    (argN 1) (argN 2) (ia !! 3)
    8 -> Just $ Equal       (argN 1) (argN 2) (ia !! 3)
    99 -> Just Halt
    _ -> Nothing
  where
    (argModes, opcode) = divMod i 100
    argN n = case nthDigit n argModes of
               0 -> Ptr (ia !! n)
               1 -> Value (ia !! n)
               x -> error ("Invalid instruction mode: " ++ show x)

-- | Run an intcode program
intcode :: [Int] -> Int -> Program -> [Int]
intcode _ _ p | null p = fail "Unexpected end of program"
intcode inputs pos xs = do
  let getValue (Value n) = n
      getValue (Ptr p) = xs ! p

  case decodeInstruction (drop pos (elems xs)) of

    Nothing -> fail ("Could not decode opcode: " ++ show (xs ! pos))

    Just (Add arg1 arg2 dest) ->
      intcode inputs (pos+4) (xs // [(dest, getValue arg1 + getValue arg2)])

    Just (Multiply arg1 arg2 dest) ->
      intcode inputs (pos+4) (xs // [(dest, getValue arg1 * getValue arg2)])

    Just (Store dest) ->
      intcode (drop 1 inputs) (pos+2) (xs // [(dest, head inputs)])

    Just (Output arg) ->
      getValue arg : intcode inputs (pos+2) xs

    Just (JumpIfTrue arg target) ->
      if getValue arg == 0
      then intcode inputs (pos+3) xs
      else intcode inputs (getValue target) xs

    Just (JumpIfFalse arg target) ->
      if getValue arg == 0
      then intcode inputs (getValue target) xs
      else intcode inputs (pos+3) xs

    Just (LessThan arg1 arg2 dest) -> do
      let v = if getValue arg1 < getValue arg2 then 1 else 0
      intcode inputs (pos+4) (xs // [(dest, v)])

    Just (Equal arg1 arg2 dest) -> do
      let v = if getValue arg1 == getValue arg2 then 1 else 0
      intcode inputs (pos+4) (xs // [(dest, v)])

    Just Halt -> []
