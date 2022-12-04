module P9a where

import Text.Megaparsec
import Text.Megaparsec.String

parseP9 :: Parser String
parseP9 = concat <$> some (parseMarker <|> parseNonMarker)
parseNonMarker = some (alphaNumChar <|> char ')')
parseMarker = do
    char '('
    bodyLength <- read <$> some digitChar
    char 'x'
    repeatN <- read <$> some digitChar
    char ')'
    body <- count bodyLength anyChar
    return $ concat $ replicate repeatN body

main = do
    indata <- readFile "p9-input.txt"
    putStr "Part 1: "
    case parse parseP9 "P9" indata of
      Right val -> print $ length val
