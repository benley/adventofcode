module P9b where

import Text.Megaparsec
import Text.Megaparsec.String

parseP9 :: Parser Int
parseP9 = sum <$> some (parseMarker <|> parseNonMarker)
parseNonMarker = length <$> some (alphaNumChar <|> char ')')
parseMarker = do
    char '('
    bodyLength <- read <$> some digitChar
    char 'x'
    repeatN <- read <$> some digitChar
    char ')'
    body <- count bodyLength anyChar
    case parse parseP9 "P9" body of
      Right val -> return $ repeatN * val

main = do
    indata <- readFile "p9-input.txt"
    putStr "Part 2: "
    case parse parseP9 "P9" indata of
      Right val -> print val
