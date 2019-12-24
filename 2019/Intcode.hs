module Intcode (intcode) where

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

intcode :: Int -> [Int] -> IO (Either String [Int])
intcode _ [] = return (Left "Unexpected end of program")
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

    Just (Output arg) -> do
      putStrLn ("Output: " ++ show (getValue arg))
      intcode (pos+2) xs

    Just (JumpIfTrue arg target) ->
      if getValue arg == 0
      then intcode (pos+3) xs
      else intcode (getValue target) xs

    Just (JumpIfFalse arg target) ->
      if getValue arg == 0
      then intcode (getValue target) xs
      else intcode (pos+3) xs

    Just (LessThan arg1 arg2 dest) -> do
      let v = if getValue arg1 < getValue arg2 then 1 else 0
      intcode (pos+4) (update dest v xs)

    Just (Equal arg1 arg2 dest) -> do
      let v = if getValue arg1 == getValue arg2 then 1 else 0
      intcode (pos+4) (update dest v xs)

    Just Halt -> return (Right xs)

-- | Update xs by storing newX at index n
-- | This is not efficient, but it's good enough for now
update :: Int -> a -> [a] -> [a]
update n newX xs = take n xs ++ [newX] ++ drop (n + 1) xs
