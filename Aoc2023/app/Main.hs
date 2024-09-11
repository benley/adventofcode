-- |

module Main where

import D1.D1 (runD1)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["d1"] -> runD1
    _ -> putStrLn "what?"
