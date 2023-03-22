module Jq.JParser where

import Parsing.Parsing
import Jq.Json

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
    s <- some  (parseEscape <|> sat (\x -> x /= '\\' && x /= '"'))
    _ <- char '"'
    return s

parseEscape :: Parser Char
parseEscape = 
    -- <|> How do we parse hex ??? "\u1234"
    fmap (const '\\') (string "\\\\") 
    <|> fmap (const '\"') (string "\\\"")
    <|> fmap (const '/') (string "\\/")
    <|> fmap (const '\b') (string "\\b")
    <|> fmap (const '\f') (string "\\f")
    <|> fmap (const '\n') (string "\\n")
    <|> fmap (const '\r') (string "\\r")  
    <|> fmap (const '\t') (string "\\t")
    <|> fmap (const '\b') (string "\\u")

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

-- >>> parse parseObject "{"asd": 5}"
-- Variable not in scope: asd
-- Perhaps you meant `and' (imported from Prelude)
