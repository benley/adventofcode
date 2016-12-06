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

isNice1 :: String -> Bool
isNice1 str = all (==True) [countVowels str >= 3,
                            hasdoubleletter str,
                            nobannedsubstrs str]

{-
  Now, a nice string is one with all of the following properties:

  It contains a pair of any two letters that appears at least twice in the
  string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like
  aaa (aa, but it overlaps).

  It contains at least one letter which repeats with exactly one letter between
  them, like xyx, abcdefeghi (efe), or even aaa.
-}

hasdoublepair :: Strool
hasdoublepair (a:b:xs) = [a,b] `isInfixOf` xs || hasdoublepair (b:xs)
hasdoublepair _ = False

type Strool = String -> Bool

aba :: Strool
aba (a:b:c:xs) = a == c || aba (b:c:xs)
aba _ = False

isNice2 :: Strool
isNice2 str = hasdoublepair str && aba str

main :: IO ()
main = do
    strs <- readFile "p5_input.txt"
    print $ length [x | x <- lines strs, isNice1 x]
    print $ length [x | x <- lines strs, isNice2 x]
