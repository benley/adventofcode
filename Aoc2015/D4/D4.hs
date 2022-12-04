module Main where

import Crypto.Hash
import qualified Data.ByteString.Char8 as BC

md5 :: String -> String
md5 bs = show (hash $ BC.pack bs :: Digest MD5)

checknum :: Int -> String -> String -> Int
checknum num seed targetstr =
    if take (length targetstr) (md5 (seed ++ show num)) == targetstr
      then num
      else checknum (num+1) seed targetstr

main :: IO ()
main = do
    putStr "Part 1: "
    print $ checknum 0 "ckczppom" "00000"
    putStr "Part 2: "
    print $ checknum 0 "ckczppom" "000000"
