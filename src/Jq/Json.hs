module Jq.Json where

data JSON = JString { str :: String} | JNumber {num :: Double} | JBool {bool :: Bool} |
  JNull | JObject {pairs :: [(String, JSON)]} | JArray {values :: [JSON]}

instance Show JSON where
  show JNull = "null"
  show (JString s) = "\"" ++s ++ "\""
  show (JNumber n) = show (truncate n)
  show (JBool b) = if b then "true" else "false"
  show (JObject p) = if null p then "{}" else "{\n  " ++ foldr (\(a,b) acc -> show a ++ ": " ++ show b ++ if acc == "" then acc else ", " ++ acc) "" p ++ "\n}"
  show (JArray arr) = if null arr then "[]" else "[\n  " ++ foldr (\a b-> show a ++ if b=="" then b else ", " ++ b) "" arr ++ "\n]"


instance Eq JSON where
  JNull == JNull = True
  (JString v) == (JString w) = v == w
  (JNumber v) == (JNumber w) = v == w
  (JBool v) == (JBool w) = v == w
  (JObject v) == (JObject w) = v == w
  (JArray v) == (JArray w) = v == w
  _ == _ = False

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
