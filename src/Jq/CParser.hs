module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser (parseStr, parseJSON)
import Jq.Json

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseWithComma

parseWithComma :: Parser Filter
parseWithComma = parseComma <|> parseArithmetic <|> parseComparisson <|> parseIf <|> parseSimpleFilters

parseSimpleFilters :: Parser Filter
-- parseSimpleFilters = parseParenthesis <|> parseRecursiveDescent <|> parseArrayIndex <|> parsePipeObjIndices  <|> parseSlice <|> parseIterator <|> parseIdentity
parseSimpleFilters = parseConstructor <|> parseTryCatch <|> parsePipeObjIndices <|> parseWithDot <|> parseParenthesis <|> parseRecursiveDescent <|> parseIdentity

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
  _ <- symbol "?"
  return $ Optional f
  <|> do
  _ <- symbol "("
  f <- parseFilter
  _ <- symbol ")"
  return f

parseObjectIndex :: Parser Filter
parseObjectIndex = do
  _ <- symbol "." 
  -- maybe has to be just a char
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
    _ <- symbol "]"
    return (ArrConst [x])
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

parsePairFilter :: Parser (Filter, Filter)
parsePairFilter = do
  name <- (do Jval . JString <$> ident) <|> (do Jval . JString <$> parseStr) <|> parseFilter
  _ <- symbol ":"
  value <- parseFilter
  return (name, value)
  <|> do
  name <- parseStr <|> ident
  return (Jval $ JString name, ObjectIndex name)

parseTryCatch :: Parser Filter
parseTryCatch = do
  _ <- symbol "try"
  f <- parseSimpleFilters
  c <- symbol "catch" <|> return ""
  if c == "" then return $ Optional f
    else do
      Try f <$> parseSimpleFilters

parseArithmetic :: Parser Filter
parseArithmetic = do
  f <- parseSimpleFilters
  s <- symbol "*" <|> symbol "+" <|> symbol "-" <|> symbol "/"
  if s == "*" then Mult f <$> parseSimpleFilters
    else if s == "+" then Plus f <$> parseSimpleFilters
    else if s == "-" then Minus f <$> parseSimpleFilters
    else Div f <$> parseSimpleFilters

parseComparisson :: Parser Filter
parseComparisson = do
  f <- parseSimpleFilters
  s <- symbol "<" <|> symbol "<=" <|> symbol ">" <|> symbol ">="
    <|> symbol "and" <|> symbol "or" <|> symbol "==" <|> symbol "!="
  if s == "<" then LessT f <$> parseSimpleFilters
  else if s == "<=" then LTEQ f <$> parseSimpleFilters
  else if s == ">" then GrT f <$> parseSimpleFilters
  else if s == ">=" then GTEQ f <$> parseSimpleFilters
  else if s == "and" then And f <$> parseSimpleFilters
  else if s == "or" then Or f <$> parseSimpleFilters
  else if s == "==" then Equiv f <$> parseSimpleFilters
  else NEquiv f <$> parseSimpleFilters

parseIf :: Parser Filter
parseIf = do
  _ <- symbol "if"
  c <- parseFilter
  _ <- symbol "then"
  t <- parseFilter
  _ <- symbol "end"
  return $ If c t Identity
  <|> do
  _ <- symbol "if"
  c <- parseFilter
  _ <- symbol "then"
  t <- parseFilter
  _ <- symbol "else"
  e <- parseFilter
  _ <- symbol "end"
  return $ If c t e


parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
      