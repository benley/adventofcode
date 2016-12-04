{-# OPTIONS_GHC -Wall #-}

module P5 where

import Data.List

{-
  A nice string is one with all of the following properties:

  It contains at least three vowels (aeiou only), like aei, xazegov,
  or aeiouaeiouaeiou.

  It contains at least one letter that appears twice in a row, like xx,
  abcdde (dd), or aabbccdd (aa, bb, cc, or dd).

  It does not contain the strings ab, cd, pq, or xy, even if they are part of
  one of the other requirements.
-}

countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiou")

hasdoubleletter :: String -> Bool
hasdoubleletter (x0:x1:xs) = (x0 == x1) || hasdoubleletter (x1:xs)
hasdoubleletter _          = False

nobannedsubstrs :: String -> Bool
nobannedsubstrs xs = not $ any (`isInfixOf` xs) bannedstrs
  where bannedstrs = ["ab", "cd", "pq", "xy"]

isNice1 str = all (==True) [countVowels str >= 3,
                            hasdoubleletter str,
                            nobannedsubstrs str]

main :: IO ()
main = do
    strs <- readFile "p5_input.txt"
    let nicestrings = [x | x <- lines strs, isNice1 x]
    print $ length nicestrings
