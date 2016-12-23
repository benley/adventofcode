-- I wanted to see if I could do the whole thing with just a parser.
-- It turns out that yeah, it is possible, but it gets to be a bit ridiculous.
module P7 where

import Control.Monad.Except (throwError)
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

data IPv7 = Addr String | Hypernet String

instance Show IPv7 where show = showVal

showVal :: IPv7 -> String
showVal (Addr x) = "Addr " ++ show x
showVal (Hypernet x) = "Net[" ++ show x ++ "]"

-- Finds the solution to part 1 almost entirely in Parsec
checkTLS :: String -> Bool
checkTLS x =
    case readOrThrow parseIPv7 x of
      Left err -> False
      Right ips -> containsAtLeastOneAbbaAddr ips && containsNoAbbaNets ips
    where
      containsAtLeastOneAbbaAddr [] = False
      containsAtLeastOneAbbaAddr (x:xs) =
          case x of
            Addr a -> case readOrThrow parseTLSAddr a of
                        Left err -> containsAtLeastOneAbbaAddr xs
                        Right _ -> True
            Hypernet _ -> containsAtLeastOneAbbaAddr xs

      containsNoAbbaNets [] = True
      containsNoAbbaNets (x:xs) =
          case x of
            Addr _ -> containsNoAbbaNets xs
            Hypernet n -> case readOrThrow parseTLSHypernet ("[" ++ n ++ "]") of
                            Left err -> False
                            Right _ -> containsNoAbbaNets xs

-- It turns out that performing arbitrary computation with Parsec is just as
-- awkward as it sounds, so this solves part 2 using normal FP
checkSSL :: String -> Bool
checkSSL x =
    case readOrThrow parseIPv7 x of
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

parseIPv7 :: P.Parser [IPv7]
parseIPv7 = P.many1 (parseAddr <|> parseHypernet)

parseAbba :: P.Parser String
parseAbba = do
    a <- P.letter
    b <- P.letter
    b' <- P.char b
    a' <- P.char a
    if a == b
      then P.unexpected "fart"
      else return [a, b, b, a]

parseNotAbba :: P.Parser Char
parseNotAbba = P.notFollowedBy parseAbba >> P.letter

parseTLSAddr :: P.Parser IPv7
parseTLSAddr = do
    a1s <- P.many (P.try parseNotAbba)
    abba <- parseAbba
    rest <- P.many P.letter
    return $ Addr (a1s ++ abba ++ rest)

parseTLSHypernet :: P.Parser IPv7
parseTLSHypernet = do
    P.char '['
    a1s <- P.many (P.try parseNotAbba)
    end <- P.choice [P.try (P.count n P.letter) | n <- [3,2,1,0]]
    P.char ']'
    return $ Hypernet (a1s ++ end)

parseAddr :: P.Parser IPv7
parseAddr = P.skipMany P.space >> Addr <$> P.many1 P.letter

parseHypernet :: P.Parser IPv7
parseHypernet = do
    P.char '['
    a <- P.many P.letter
    P.char ']'
    return $ Hypernet a

data IpError = Parser P.ParseError

instance Show IpError where show = showError

showError :: IpError -> String
showError (Parser x) = "Parse error at " ++ show x

type ThrowsError = Either IpError

readOrThrow :: P.Parser a -> String -> ThrowsError a
readOrThrow parser x =
    case P.parse parser "herp derp" x of
      Left  err -> throwError (Parser err)
      Right val -> return val

main = do
  indata <- lines <$> readFile "p7-input.txt"
  putStrLn $ "Part 1 (support TLS): " ++ show (length $ filter checkTLS indata)
  putStrLn $ "Part 2 (support SSL): " ++ show (length $ filter checkSSL indata)
