#!/usr/bin/env runhaskell

module D2p2_2018 where

dist :: Eq a => [a] -> [a] -> Int
dist a b
    = last (if lab == 0 then mainDiag
            else if lab > 0 then lowers !! (lab - 1)
                 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag a [] diags = []
          eachDiag a (bch:bs) (lastDiag:diags) =
              oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
              where nextDiag = head (tail diags)
          oneDiag a b diagAbove diagBelow = thisdiag
              where doDiag [] b nw n w = []
                    doDiag a [] nw n w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z

findMatch :: [String] -> Maybe (String, String)
findMatch (x:y:xs) = if dist x y == 1 then Just (x, y) else findMatch (x:xs)
findMatch _ = Nothing

findOuter :: [String] -> (String, String)
findOuter (x:xs) = let res = findMatch (x:xs) in
  case res of
    Nothing -> findOuter xs
    Just m -> m
findOuter _ = error "nope"

findSame :: String -> String -> String
findSame (x:xs) (y:ys) = if x == y then (x : findSame xs ys) else findSame xs ys
findSame _ _ = []

main :: IO ()
main = do
  tokens <- lines <$> getContents
  let (m1, m2) = findOuter tokens
  putStrLn $ findSame m1 m2
