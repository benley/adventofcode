{-# LANGUAGE NamedFieldPuns #-}

module Intcode (
  intcode,
  newVm,
  -- runIntcode,
  VmState(..),
  Program
) where

import Control.Concurrent.STM.TBQueue
import GHC.Conc.Sync (STM)
-- import Control.Monad.Trans.State
-- import Data.DList as DL

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

type Program = [Int]

-- return the nth digit (from the right) of an integer, starting at 1
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

data VmState = VmState { inputs :: TBQueue Int
                       , outputs :: TBQueue Int
                       , position :: Int
                       , program :: Program
                       } -- deriving Show

newVm :: Program -> STM VmState
newVm p = do
  outputs <- newTBQueue 100
  inputs <- newTBQueue 100
  return VmState { inputs = inputs
                 , outputs = outputs
                 , position = 0
                 , program = p }

-- runIntcode :: [Int] -> State VmState [Int]
-- runIntcode inputs = do
--   vm <- get
--   case intcode vm{inputs=inputs} of
--     Left err -> fail ("HERP DERP " ++ err)
--     Right vm' -> do
--       outList <- flushTQueue $ outputs vm'
--       put vm'
--       return outList

-- intcode :: VmState -> Either String VmState
-- intcode :: VmState -> STM VmState
-- intcode :: VmState -> STM VmState
intcode :: VmState -> STM ()
intcode VmState{program = []} = fail "Unexpected end of program"

intcode vm@VmState{inputs, outputs, position = pos, program = xs} = do
  let getValue (Value n) = n
      getValue (Ptr p) = xs !! p

  case decodeInstruction (drop pos xs) of

    Nothing -> fail ("Could not decode opcode: " ++ show (xs !! pos))

    Just (Add arg1 arg2 dest) ->
      intcode vm { position = pos+4
                 , program = update dest (getValue arg1 + getValue arg2) xs }

    Just (Multiply arg1 arg2 dest) ->
      intcode vm { position = pos+4
                 , program = update dest (getValue arg1 * getValue arg2) xs }

    Just (Store dest) -> do
      nextInput <- readTBQueue inputs
      intcode vm { position = pos+2
                 , program = update dest nextInput xs }

    Just (Output arg) -> do
      writeTBQueue outputs (getValue arg)
      intcode vm { position = pos+2 }

    Just (JumpIfTrue arg target) ->
      if getValue arg == 0
      then intcode vm { position = pos+3 }
      else intcode vm { position = getValue target }

    Just (JumpIfFalse arg target) ->
      if getValue arg == 0
      then intcode vm { position = getValue target }
      else intcode vm { position = pos+3 }

    Just (LessThan arg1 arg2 dest) -> do
      let v = if getValue arg1 < getValue arg2 then 1 else 0
      intcode vm { position = pos+4
                 , program = update dest v xs }

    Just (Equal arg1 arg2 dest) -> do
      let v = if getValue arg1 == getValue arg2 then 1 else 0
      intcode vm { position = pos+4
                 , program = update dest v xs }

    Just Halt -> return ()

-- | Update xs by storing newX at index n
-- | This is not efficient, but it's good enough for now
update :: Int -> a -> [a] -> [a]
update n newX xs = take n xs ++ [newX] ++ drop (n + 1) xs
