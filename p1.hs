{-# OPTIONS_GHC -Wall #-}
module Main where

import Text.Printf

santaFloor :: Int -> String -> Int
santaFloor flr [] = flr
santaFloor flr (p:parens)
    | p == '(' = santaFloor (flr + 1) parens
    | p == ')' = santaFloor (flr - 1) parens
    | otherwise = santaFloor flr parens

firstNegative :: Int -> Int -> String -> Int
firstNegative pos flr []
    | flr < 0 = pos
    | otherwise = error "never went negative"
firstNegative pos flr (p:parens)
    | flr < 0 = pos
    | p == '(' = firstNegative (pos+1) (flr+1) parens
    | p == ')' = firstNegative (pos+1) (flr-1) parens
    | otherwise = firstNegative (pos+1) flr parens

main :: IO ()
main = do
    input <- getContents
    printf "Went negative at: %d\n" $ firstNegative 0 0 input
    printf "Final floor: %d\n" $ santaFloor 0 input
