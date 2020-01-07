{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Intcode (
  intcode,
  toProgram,
  progFromFile,
  newVm,
  runIntcode,
  VmState(..),
  Program
) where

import Data.Array
import Data.Text (strip, unpack, splitOn)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

type Address = Int

data Arg = Value Int
         | Ptr Address
         | Relative Address
         deriving Show

data Instruction = Add Arg Arg Address
                 | Multiply Arg Arg Address
                 | Store Address
                 | Output Arg
                 | JumpIfTrue Arg Arg
                 | JumpIfFalse Arg Arg
                 | LessThan Arg Arg Address
                 | Equal Arg Arg Address
                 | SetRelBase Arg
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
    9 -> Just $ SetRelBase  (argN 1)
    99 -> Just Halt
    _ -> Nothing
  where
    (argModes, opcode) = divMod i 100
    argN n = case nthDigit n argModes of
               0 -> Ptr (ia !! n)
               1 -> Value (ia !! n)
               2 -> Relative (ia !! n)
               x -> error ("Invalid instruction mode: " ++ show x)

data VmState = VmState { position :: Address
                       , relBase :: Address
                       , program :: Program
                       }

newVm :: VmState
newVm = VmState { position = 0
                , relBase = 0
                , program = undefined
                }

-- | Run an intcode program with input
runIntcode :: [Int] -> [Int] -> [Int]
runIntcode inputs prog = intcode inputs (newVm { program = toProgram prog })

-- | Run an intcode VM
intcode :: [Int] -> VmState -> [Int]

intcode _ VmState{program} | null program = fail "Unexpected end of program"

intcode inputs vm@VmState{position=pos, program=xs, relBase} = do
  let getValue (Value n) = n
      getValue (Ptr p) = xs ! p
      getValue (Relative r) = xs ! (r + relBase)

  case decodeInstruction (drop pos (elems xs)) of

    Nothing -> error ("Could not decode opcode: " ++ show (xs ! pos))

    Just (Add arg1 arg2 dest) ->
      intcode inputs vm { position = pos+4
                        , program = xs // [(dest, getValue arg1 + getValue arg2)] }

    Just (Multiply arg1 arg2 dest) ->
      intcode inputs vm { position = pos+4
                        , program = xs // [(dest, getValue arg1 * getValue arg2)] }

    Just (Store dest) ->
      intcode (drop 1 inputs) vm { position = pos+2
                                 , program = xs // [(dest, head inputs)] }

    Just (Output arg) ->
      getValue arg : intcode inputs vm { position = pos+2 }

    Just (JumpIfTrue arg target) ->
      if getValue arg == 0
      then intcode inputs vm { position = pos+3 }
      else intcode inputs vm { position = getValue target }

    Just (JumpIfFalse arg target) ->
      if getValue arg == 0
      then intcode inputs vm { position = getValue target }
      else intcode inputs vm { position = pos+3 }

    Just (LessThan arg1 arg2 dest) -> do
      let v = if getValue arg1 < getValue arg2 then 1 else 0
      intcode inputs vm { position = pos+4, program = xs // [(dest, v)] }

    Just (Equal arg1 arg2 dest) -> do
      let v = if getValue arg1 == getValue arg2 then 1 else 0
      intcode inputs vm { position = pos+4, program = xs // [(dest, v)] }

    Just (SetRelBase arg) ->
      intcode inputs vm { position = pos+2, relBase = relBase + getValue arg }

    Just Halt -> []
