{-# LANGUAGE OverloadedStrings #-}
module P5b where

import Data.List (sortOn)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5 (md5)

inputHash = "ugkcyxxp"

pooploop :: Int -> [(Char, Char)] -> [(Char, Char)]
pooploop n acc
  | length acc >= 8 = acc
  | otherwise =
      if take 5 hash == "00000"
           && (hash !! 5) `elem` ("01234567" :: String)
           && (hash !! 5) `notElem` map fst acc
        then pooploop (n + 1) ((hash !! 5, hash !! 6) : acc)
        else pooploop (n + 1) acc
      where
        doorId = LB.concat [inputHash, Builder.toLazyByteString (Builder.intDec n)]
        hash = show (md5 doorId)

-- pooploop is going to return this:
precomputed = [('3','7'),('6','e'),('0','f'),('7','5'),('5','0'),('1','2'),('2','c'),('4','3')]

main :: IO ()
main = putStrLn $ map snd $ sortOn fst $ pooploop 700000 []
