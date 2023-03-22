module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Data.Char ( chr )

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseBool :: Parser JSON
parseBool = do
    b <- string "true" <|> string "false"
    if b == "true" then return (JBool True) else return (JBool False)

parseNum :: Parser JSON
parseNum = parseScientific <|> JNumber <$> parseFloat <|> parseNormal

parseNormal :: Parser JSON
parseNormal = JNumber . fromIntegral <$> int

parseFloat :: Parser Double
parseFloat =  do
    n <- int
    _ <- symbol "."
    fraction <- some digit
    return (read (show n ++ "." ++ fraction))

parseScientific :: Parser JSON
parseScientific = do
    n <- parseFloat
    _ <- char 'e' <|> char 'E'
    sign <- symbol "+" <|> symbol "-" <|> symbol ""
    mag <- int
    if sign == "-" then return (JNumber (n / 10 ^ mag))  else return (JNumber (n * 10 ^ mag))

parseString :: Parser JSON
parseString = JString <$> parseStr

parseStr :: Parser String
parseStr = do
    _ <- char '"'
    s <- some  (sat (\x -> x /= '\\' && x /= '"')  <|> parseEscape)
    _ <- char '"'
    return s

escapeUnicode :: Parser Char
escapeUnicode = parseUni >>= return . chr . read

-- >>> parse parseStr "\"\\u1234\""
-- [("\4660","")]

parseEscape :: Parser Char
parseEscape =
    fmap (const '\\') (string "\\\\")
    <|> fmap (const '\"') (string "\\\"")
    <|> fmap (const '/') (string "\\/")
    <|> fmap (const '\b') (string "\\b")
    <|> fmap (const '\f') (string "\\f")
    <|> fmap (const '\n') (string "\\n")
    <|> fmap (const '\r') (string "\\r")
    <|> fmap (const '\t') (string "\\t")
    <|> escapeUnicode

parseUni :: Parser String
parseUni = do
    _ <- string "\\u"
    a <- digit <|> sat (`elem` "abcdefABCDEF")
    b <- digit <|> sat (`elem` "abcdefABCDEF")
    c <- digit <|> sat (`elem` "abcdefABCDEF")
    d <- digit <|> sat (`elem` "abcdefABCDEF")
    return (show (parseHex [a,b,c,d]))

parseHex :: String -> Integer
parseHex [] = 0
parseHex hxStr = convertHex (last hxStr)+(16*parseHex (init hxStr))

convertHex :: Char -> Integer
convertHex hex
    | hex == '0' = 0
    | hex == '1' = 1
    | hex == '2' = 2
    | hex == '3' = 3
    | hex == '4' = 4
    | hex == '5' = 5
    | hex == '6' = 6
    | hex == '7' = 7
    | hex == '8' = 8
    | hex == '9' = 9
    | hex == 'A' || hex == 'a' = 10
    | hex == 'B' || hex == 'b' = 11
    | hex == 'C' || hex == 'c' = 12
    | hex == 'D' || hex == 'd' = 13
    | hex == 'E' || hex == 'e' = 14
    | hex == 'F' || hex == 'f' = 15
    | otherwise     = 0

parseArray :: Parser JSON
parseArray = do
    _ <- symbol "["
    x <- many parseJSON
    xs <- many (do
        _ <- symbol ","
        parseJSON)
    _ <- symbol "]"
    return (JArray (x++xs))

parseObject :: Parser JSON
parseObject = do
    _ <- symbol "{"
    x <- many parsePair
    xs <- many (do
        _ <- symbol ","
        parsePair)
    _ <- symbol "}"
    return (JObject (x ++ xs))

parsePair :: Parser (String, JSON)
parsePair = do
    s <- parseStr
    _ <- char ':'
    json <- parseNum
    return (s, json)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseBool <|> parseNum <|> parseString <|> parseArray <|> parseObject

-- >>> parse parseObject `{"foo": "42", "bar": "43"}`
-- parse error on input `{'
