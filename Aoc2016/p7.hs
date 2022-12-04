-- I wanted to see if I could do the whole thing with just a parser.
-- It turns out that yeah, it is possible, but it gets to be a bit ridiculous.
module P7 where

import Control.Monad.Except (throwError)
import Text.Megaparsec
import Text.Megaparsec.String

data IPv7 = Addr String | Hypernet String deriving Show

-- Finds the solution to part 1 almost entirely in Parsec
checkTLS :: String -> Bool
checkTLS x =
    case parse parseIPv7 "p7" x of
      Left err -> False
      Right ips -> containsAtLeastOneAbbaAddr ips && containsNoAbbaNets ips
    where
      containsAtLeastOneAbbaAddr [] = False
      containsAtLeastOneAbbaAddr (x:xs) =
          case x of
            Addr a -> case parse parseTLSAddr "p7" a of
                        Left err -> containsAtLeastOneAbbaAddr xs
                        Right _ -> True
            Hypernet _ -> containsAtLeastOneAbbaAddr xs

      containsNoAbbaNets [] = True
      containsNoAbbaNets (x:xs) =
          case x of
            Addr _ -> containsNoAbbaNets xs
            Hypernet n -> case parse parseTLSHypernet "p7" ("[" ++ n ++ "]") of
                            Left err -> False
                            Right _ -> containsNoAbbaNets xs

-- It turns out that performing arbitrary computation with Parsec is just as
-- awkward as it sounds, so this solves part 2 using normal FP
checkSSL :: String -> Bool
checkSSL x =
    case parse parseIPv7 "p7" x of
      Left  err -> False
      Right ips -> any (`rcontainsBAB` ips) (rfindABA ips)
    where
      rfindABA ips = concatMap findAllABA [a | Addr a <- ips]

      findAllABA :: String -> [String]
      findAllABA (a:b:c:xs) =
          if a == c && a /= b
            then [a,b,a] : findAllABA (b:c:xs)
            else findAllABA (b:c:xs)
      findAllABA _ = []

      rcontainsBAB :: String -> [IPv7] -> Bool
      rcontainsBAB aba ips = any (containsBAB aba) [n | Hypernet n <- ips]

      containsBAB :: String -> String -> Bool
      containsBAB (a:b:_) (bb:aa:bb':xs) =
          bb == b && aa == a && bb' == b || containsBAB [a,b,a] (aa:bb':xs)
      containsBAB _ _ = False

parseIPv7 :: Parser [IPv7]
parseIPv7 = some (parseAddr <|> parseHypernet)

parseAbba :: Parser String
parseAbba = do
    a <- letterChar
    b <- letterChar
    b' <- char b
    a' <- char a
    if a == b
      then unexpected EndOfInput
      else return [a, b, b, a]

parseNotAbba :: Parser Char
parseNotAbba = notFollowedBy parseAbba >> letterChar

parseTLSAddr :: Parser IPv7
parseTLSAddr = do
    a1s <- many (try parseNotAbba)
    abba <- parseAbba
    rest <- many letterChar
    return $ Addr (a1s ++ abba ++ rest)

parseTLSHypernet :: Parser IPv7
parseTLSHypernet = do
    char '['
    a1s <- many (try parseNotAbba)
    end <- choice [try (count n letterChar) | n <- [3,2,1,0]]
    char ']'
    return $ Hypernet (a1s ++ end)

parseAddr :: Parser IPv7
parseAddr = Addr <$> some letterChar

parseHypernet :: Parser IPv7
parseHypernet = do
    char '['
    a <- some letterChar
    char ']'
    return $ Hypernet a

main = do
  indata <- lines <$> readFile "p7-input.txt"
  putStrLn $ "Part 1 (support TLS): " ++ show (length $ filter checkTLS indata)
  putStrLn $ "Part 2 (support SSL): " ++ show (length $ filter checkSSL indata)
