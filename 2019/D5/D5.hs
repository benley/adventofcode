{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

-- | This will probably need to return useful errors at some point, but
-- | for now it can just return [-1] if it encounters unknown opcodes
intcode :: Int -> [Int] -> IO [Int]

-- opcode 99: halt
intcode pos xs | xs!!pos == 99 = return xs

-- | opcode 1: add a b dest
-- | opcode 2: multiply a b dest
intcode pos xs | opcode == 1 = intcode (pos+4) (update dest (xs!!a + xs!!b) xs)
               | opcode == 2 = intcode (pos+4) (update dest (xs!!a * xs!!b) xs)
  where (opcode:a:b:dest:_) = drop pos xs

-- | opcode 3: store dest n
intcode pos xs | opcode == 3 = intcode (pos+2) (update dest n xs)
  where (opcode:dest:n:_) = drop pos xs

-- | opcode 4: output n
intcode pos xs | opcode == 4 = do
                   print n
                   intcode (pos+2) xs
  where (opcode:n:_) = drop pos xs

intcode _ _ = return [-1]

-- | This is not efficient, but it's good enough for now
update :: Int -> a -> [a] -> [a]
update n newX xs = take n xs ++ [newX] ++ drop (n + 1) xs

main :: IO ()
main = undefined
