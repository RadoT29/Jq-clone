module Jq.Json where
import Data.List (sortBy)
import Data.Function (on)

data JSON = JString { str :: String} | JNumber {num :: Double} | JBool {bool :: Bool} |
  JNull | JObject {pairs :: [(String, JSON)]} | JArray {values :: [JSON]}

instance Show JSON where
  show JNull = "null"
  show (JString s) = "\"" ++s ++ "\""
  show (JNumber n) = if n == fromInteger (round n) then show (round n :: Integer) else show n
  show (JBool b) = if b then "true" else "false"
  show other = showNested other 0

showNested :: JSON -> Int -> String
showNested (JArray []) _ = "[]"
showNested (JArray arr) n  = "[\n" ++ concatMap (const "  ") [0..n] ++ foldr (\a b-> showNested a (n+1) ++ if b=="" then b else ",\n" ++ concatMap (const "  ") [0..n] ++ b) "" arr ++ "\n" ++ concatMap (const "  ") [0..n-1] ++"]"
showNested (JObject []) _ = "{}"
showNested (JObject p) n = "{\n" ++  concatMap (const "  ") [0..n] ++ foldr (\(a,b) acc -> show (JString a) ++ ": " ++ showNested b (n+1) ++ if acc == "" then acc else ",\n" ++  concatMap (const "  ") [0..n] ++ acc) "" p ++ "\n" ++ concatMap (const "  ") [0..n-1] ++ "}"
showNested x _ = show x

instance Eq JSON where
  JNull == JNull = True
  (JString v) == (JString w) = v == w
  (JNumber v) == (JNumber w) = v == w
  (JBool v) == (JBool w) = v == w
  (JObject v) == (JObject w) = sortBy (compare `on` fst) v == sortBy (compare `on` fst) w 
  (JArray v) == (JArray w) = v == w
  _ == _ = False

instance Ord JSON where
  JNull <= _ = True
  _ <= JNull = False
  (JBool v) <= (JBool w) = v <= w
  (JNumber v) <= (JNumber w) = v <= w
  (JString v) <= (JString w) = v <= w
  (JArray []) <= (JArray _) = True
  (JArray _) <= (JArray []) = False
  (JArray (v:_)) <= (JArray (w:_)) = v <= w
  (JObject v) <= (JObject w) = if map fst (sortBy (compare `on` fst) v) == map fst (sortBy (compare `on` fst) w) 
    then map snd (sortBy (compare `on` fst) v) <= map snd (sortBy (compare `on` fst) w) else map fst (sortBy (compare `on` fst) v) <= map fst (sortBy (compare `on` fst) w)
  (JBool _) <= _ = True
  _ <= (JBool _) = False
  (JNumber _) <= _ = True
  _ <= (JNumber _) = False
  (JString _) <= _ = True
  _ <= (JString _) = False
  (JArray _) <= _ = True
  _ <= (JArray _) = False

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC n = JNumber (fromIntegral n)

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
