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
    n <- parseFloat <|> fromIntegral <$> int
    _ <- char 'e' <|> char 'E'
    sign <- symbol "+" <|> symbol "-" <|> symbol ""
    mag <- nat
    if sign == "-" then return (JNumber (n / 10 ^ mag))  else return (JNumber (n * 10 ^ mag))

parseString :: Parser JSON
parseString = JString <$> parseStr

parseStr :: Parser String
parseStr = do
    _ <- char '"'
    actualString

actualString :: Parser String
actualString = do
    s <- item <|> escapeUnicode
    if s == '"' then do return ""
    else if s == '\\' then do
        e <- parseEscape <|> char 'u'
        if e == 'u' then do
            hex <- escapeUnicode
            xs <- actualString
            return $ hex :xs
        else 
            do
            xs <- actualString
            return $ s : e : xs
    else do
        xs <- actualString
        return $ s : xs

escapeUnicode :: Parser Char
escapeUnicode = parseUni >>= return . chr . read

parseEscape :: Parser Char
parseEscape = do
    fmap (const '\\') (char '\\')
    <|> fmap (const '\"') (char '\"')
    <|> fmap (const '/') (char '/')
    <|> fmap (const 'b') (char 'b')
    <|> fmap (const 'f') (char 'f')
    <|> fmap (const 'n') (char 'n')
    <|> fmap (const 'r') (char 'r')
    <|> fmap (const 't') (char 't')
    
parseUni :: Parser String
parseUni = do
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
parseArray = emptyArr <|> filledArr

emptyArr :: Parser JSON
emptyArr = do
    _ <- symbol "["
    _ <- symbol "]"
    return (JArray [])

filledArr :: Parser JSON
filledArr = do
    _ <- symbol "["
    x <- parseJSON
    xs <- many (do
        _ <- symbol ","
        parseJSON)
    _ <- symbol "]"
    return (JArray (x:xs))

parseObject :: Parser JSON
parseObject = emptyObj <|> filledObj

emptyObj :: Parser JSON
emptyObj = do
    _ <- symbol "{"
    _ <- symbol "}"
    return (JObject [])

filledObj :: Parser JSON
filledObj =  do
    _ <- symbol "{"
    x <- parsePair
    xs <- many (do
        _ <- symbol ","
        parsePair)
    _ <- symbol "}"
    return (JObject (x : xs))

parsePair :: Parser (String, JSON)
parsePair = do
    s <- parseStr
    _ <- symbol ":"
    json <- parseJSON
    return (s, json)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseBool <|> parseNum <|> parseString <|> parseArray <|> parseObject