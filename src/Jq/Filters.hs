module Jq.Filters where
import Jq.Json ( JSON )

data Filter = Identity | Parenthesis Filter | ObjectIndex String
 | ArrayIndex Int | Slice Int Int | Jval JSON | ArrConst [Filter] | ObjConst [(String, Filter)]
 | Iterator [Either Int String] | EmptyIteration | Optional Filter
 | Pipe Filter Filter | Comma Filter Filter | RecursiveDescent | Try Filter Filter
 | Plus Filter Filter | Minus Filter Filter | Mult Filter Filter | Div Filter Filter
 | Equiv Filter Filter | NEquiv Filter Filter | LessT Filter Filter | GrT Filter Filter
 | GTEQ Filter Filter | LTEQ Filter Filter | And Filter Filter | Or Filter Filter | Not
 | If Filter Filter Filter
 

instance Show Filter where
  show Identity = "."
  show (ObjectIndex str) = str
  show (Pipe a b) = show a ++ " | " ++ show b
  show (Comma a b) = show a ++ ", " ++ show b
  show (ArrayIndex i) = ".[" ++ show i ++ "]"
  show (Slice l r) = ".[" ++ show l ++ show r ++ "]"
  show (Iterator _) = ".[]"
  show (Optional f) = show f ++ "?"
  show (Parenthesis f) = "(" ++ show f ++ ")"
  show RecursiveDescent = ".."
  show EmptyIteration = "[]"
  show (Jval v) = show v
  show (ArrConst f) = "ARRCONST" ++ show f
  show (ObjConst f) = "OBJCONST" ++ show f
  show (Try t c) = "Try " ++ show t ++ "Catch " ++ show c
  show (Plus a b) = show a ++ " + " ++ show b
  show (Minus a b) = show a ++ " - " ++ show b
  show (Mult a b) = show a ++ " * " ++ show b
  show (Div a b) = show a ++ " / " ++ show b


instance Eq Filter where
  Identity == Identity = True
  (ObjectIndex a) == (ObjectIndex b) = a == b
  (Pipe a b) == (Pipe x y) = a == x && b == y
  (Comma a b) == (Comma x y) = a == x && b == y
  (Parenthesis f) == (Parenthesis v) = f == v
  (ArrayIndex i) == (ArrayIndex j) = i == j
  (Slice a b) == (Slice c d) = a == c && b == d 
  (Iterator xs) == (Iterator ys) = xs == ys
  _ == _ = False

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = ObjectIndex

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma