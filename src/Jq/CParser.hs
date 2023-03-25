module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser (parseStr, parseJSON)

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseWithComma

parseWithComma :: Parser Filter
parseWithComma = parseComma <|> parseSimpleFilters

parseSimpleFilters :: Parser Filter
-- parseSimpleFilters = parseParenthesis <|> parseRecursiveDescent <|> parseArrayIndex <|> parsePipeObjIndices  <|> parseSlice <|> parseIterator <|> parseIdentity
parseSimpleFilters = parseConstructor <|> parsePipeObjIndices <|> parseWithDot <|> parseParenthesis <|> parseRecursiveDescent <|> parseIdentity

parseWithDot :: Parser Filter
parseWithDot = do
  _ <- symbol "."
  parseWithoutDot

parseWithoutDot :: Parser Filter
parseWithoutDot = do
  parseArrayIndex <|> parseObjectIndexBrackets <|> parseSlice <|> parseIterator

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
  Comma left <$> parseWithComma

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- symbol "("
  f <- parseFilter
  _ <- symbol ")"
  return f

parseObjectIndex :: Parser Filter
parseObjectIndex = do
  _ <- symbol "."
  identity <- ident <|> parseStr
  _ <- symbol "?"
  return $ Optional $ ObjectIndex identity
  <|> do
  _ <- symbol "."
  ObjectIndex <$> (ident <|> parseStr)

parseObjectIndexBrackets :: Parser Filter
parseObjectIndexBrackets = do
  s <- parseSquareBrackets parseStr
  _ <- symbol "?"
  return $ Optional $ ObjectIndex s
  <|> do
  s <- parseSquareBrackets parseStr
  return $ ObjectIndex s

parsePipeObjIndices :: Parser Filter
parsePipeObjIndices = do
  left <- parseObjectIndex <|> parseWithDot
  Pipe left <$> (pipeHelper <|> return Identity)

pipeHelper :: Parser Filter
pipeHelper = do
  left <- parseObjectIndex <|> parseWithoutDot
  right <- pipeHelper <|> return Identity
  return $ Pipe left right

parseArrayIndex :: Parser Filter
parseArrayIndex = do
  s <- parseSquareBrackets int
  _ <- symbol "?"
  return $ Optional $ ArrayIndex s
  <|> do
  s <- parseSquareBrackets int
  return $ ArrayIndex s

parseSquareBrackets :: Parser a -> Parser a
parseSquareBrackets p = do
  -- _ <- symbol "."
  _ <- symbol "["
  s <- p
  _ <- symbol "]"
  return s

parseSlice :: Parser Filter
parseSlice = do
  (lowerBound, upperBound) <- parseSquareBrackets parseBounds
  _ <- symbol "?"
  return $ Optional $ Slice lowerBound upperBound
  <|> do
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
  _ <- symbol "?"
  return $ Optional EmptyIteration
  <|> do
  _ <- parseSquareBrackets (string "")
  return EmptyIteration
  <|> do
  l <- parseSquareBrackets (parseIteratorIndices ((Left <$> int) <|> (Right <$> parseStr)))
  _ <- symbol "?"
  return $ Optional $ Iterator l
  <|> do
  l <- parseSquareBrackets (parseIteratorIndices ((Left <$> int) <|> (Right <$> parseStr)))
  return $ Iterator l

parseIteratorIndices :: Parser (Either Int String) -> Parser [Either Int String]
parseIteratorIndices p = do
  x <- p
  xs <- many (do
      _ <- symbol ","
      p)
  return (x:xs)

parseRecursiveDescent :: Parser Filter
parseRecursiveDescent = do
  _ <- symbol ".."
  return RecursiveDescent

parseConstructor :: Parser Filter
parseConstructor = do
  Jval <$> parseJSON
  <|> do
    _ <- symbol "["
    _ <- symbol "]"
    return (ArrConst [])
  <|> do
    _ <- symbol "["
    x <- parseFilter
    xs <- many (do
        _ <- symbol ","
        parseFilter)
    _ <- symbol "]"
    return (ArrConst (x:xs))
    <|> do
    _ <- symbol "{"
    _ <- symbol "}"
    return (ObjConst [])
    <|> do
    _ <- symbol "{"
    x <- parsePairFilter
    xs <- many (do
        _ <- symbol ","
        parsePairFilter)
    _ <- symbol "}"
    return (ObjConst (x:xs))

parsePairFilter :: Parser (String, Filter)
parsePairFilter = do
  name <- parseStr <|> ident
  _ <- symbol ":"
  value <- parseFilter
  return (name, value)
  <|> do
  name <- parseStr <|> ident
  return (name, ObjectIndex name)

parseTryCatch :: Parser Filter
parseTryCatch = do
  _ <- symbol "try"
  f <- parseFilter
  c <- symbol "catch" <|> return ""
  if c == "" then return $ Optional f
    else do
      Try f <$> parseFilter

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
