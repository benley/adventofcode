{-# LANGUAGE OverloadedStrings #-}
module P5a where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5 (md5)

inputHash = "ugkcyxxp"

pooploop n =
    if take 5 hash == "00000"
      then hash !! 5 : pooploop (n + 1)
      else pooploop (n + 1)
    where
      doorId = LB.concat [inputHash, Builder.toLazyByteString (Builder.intDec n)]
      hash = show (md5 doorId)

main :: IO ()
main = putStrLn $ take 8 $ pooploop 700000
