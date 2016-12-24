module P9 where

import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

parseP9 :: P.Parser String
parseP9 = concat <$> P.many1 (parseMarker <|> parseNonMarker)

parseNonMarker :: P.Parser String
parseNonMarker = P.many1 (P.alphaNum <|> P.char ')')

parseMarker :: P.Parser String
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
      Left err -> print "shit dawg"
      Right val -> print $ length val
