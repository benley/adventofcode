module P9a where

import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

parseP9 = concat <$> P.many1 (parseMarker <|> parseNonMarker)
parseNonMarker = P.many1 (P.alphaNum <|> P.char ')')
parseMarker = do
    P.char '('
    bodyLength <- read <$> P.many1 P.digit
    P.char 'x'
    repeatN <- read <$> P.many1 P.digit
    P.char ')'
    body <- P.count bodyLength P.anyChar
    return $ concat $ replicate repeatN body

main = do
    indata <- readFile "p9-input.txt"
    putStr "Part 1: "
    case P.parse parseP9 "P9" indata of
      Right val -> print $ length val
