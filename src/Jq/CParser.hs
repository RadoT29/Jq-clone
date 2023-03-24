module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser (parseStr)

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseWithComma

parseWithComma :: Parser Filter
parseWithComma = parseComma <|> parseSimpleFilters

parseSimpleFilters :: Parser Filter
parseSimpleFilters = parseIdentity <|> parseParenthesis <|> parseObjectIndex <|> parseArrayIndex <|> parseSlice <|> parseIterator

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parsePipe :: Parser Filter
parsePipe = do
  left <- parseWithComma
  _ <- symbol "|"
  Pipe left <$> parseFilter

parseComma :: Parser Filter
parseComma = do 
  left <- parseSimpleFilters
  _ <- symbol ","
  Comma left <$> parseFilter

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- symbol "("
  f <- parseFilter
  _ <- symbol ")"
  return f

parseObjectIndex :: Parser Filter
parseObjectIndex = do
  s <- parseSquareBrackets parseStr
  return $ ObjectIndex s
  <|> do
  _ <- symbol "."
  ObjectIndex <$> ident
  <|> do
  _ <- symbol "."
  identity <- ident
  _ <- symbol "?"
  return $ Optional $ ObjectIndex identity

parseArrayIndex :: Parser Filter
parseArrayIndex = do
  s <- parseSquareBrackets int
  return $ ArrayIndex s

parseSquareBrackets :: Parser a -> Parser a
parseSquareBrackets p = do
  _ <- symbol "."
  _ <- symbol "["
  s <- p
  _ <- symbol "]"
  return s

parseSlice :: Parser Filter
parseSlice = do
  (lowerBound, upperBound) <- parseSquareBrackets parseBounds
  return $ Slice lowerBound upperBound

parseBounds :: Parser (Int, Int)
parseBounds = do
  lowerBound <- int <|> return 0
  _ <- symbol ":"
  upperBound <- int <|> return (maxBound :: Int)
  return (lowerBound, upperBound)

parseIterator :: Parser Filter
parseIterator = do
  _ <- parseSquareBrackets (string "")
  return $ Iterator []
  <|> do
  _ <- parseSquareBrackets (string "")
  _ <- symbol "?"
  return $ Optional $ Iterator []
  <|> do
  l <- parseSquareBrackets (parseIteratorIndices int)
  return $ Iterator l
  <|> do
  l <- parseSquareBrackets (parseIteratorIndices int)
  _ <- symbol "?"
  return $ Optional $ Iterator l
  <|> do
  l <- parseSquareBrackets (parseIteratorIndices parseStr)
  return $ IteratorObj l
  <|> do
  l <- parseSquareBrackets (parseIteratorIndices parseStr)
  _ <- symbol "?"
  return $ Optional $ IteratorObj l

parseIteratorIndices :: Parser a -> Parser [a]
parseIteratorIndices p = do
  x <- p
  xs <- many (do
      _ <- symbol ","
      p)
  return (x:xs)



parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
