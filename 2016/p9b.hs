module P9b where

import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

parseP9 = sum <$> P.many1 (parseMarker <|> parseNonMarker)
parseNonMarker = length <$> P.many1 (P.alphaNum <|> P.char ')')
parseMarker = do
    P.char '('
    bodyLength <- read <$> P.many1 P.digit
    P.char 'x'
    repeatN <- read <$> P.many1 P.digit
    P.char ')'
    body <- P.count bodyLength P.anyChar
    case parse body of
      Right val -> return $ repeatN * val

parse = P.parse parseP9 "P9"

main = do
    indata <- readFile "p9-input.txt"
    putStr "Part 2: "
    case parse indata of
      Right val -> print val
