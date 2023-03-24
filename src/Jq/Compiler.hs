module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (Parenthesis a) inp = compile a inp
compile (ObjectIndex s) inp = objectIndex s inp
compile (Pipe a b) inp = compile a inp >>= (fmap concat . mapM (compile b))
compile (Comma a b) inp = compile a inp >>= (\x -> fmap (x ++)  (compile b inp))
compile (Optional f) inp = let output = compile f inp
    in case output of
        (Left _) -> return []
        right -> right
compile (ArrayIndex index) inp = arrayIndex index inp
compile (Slice lower upper) inp = case inp of
    (JArray arr) | length arr < lower  -> Right []
                 | length arr < upper -> Right (take (length arr - lower) (drop lower arr))
                 | otherwise -> Right (take (upper - lower) (drop lower arr))
    JNull ->Right [JNull]
    _ -> Left "An Array has to be provided"
compile (Iterator indices) inp = case inp of
    (JArray arr)   | null indices -> Right arr
                   | otherwise    -> fmap concat  (mapM (\x -> arrayIndex x (JArray arr)) indices)
    _                             -> Left "Iterator only works with Arrays"
compile (IteratorObj indices) inp = case inp of
    (JObject dict) | null indices -> Right (map snd dict)
                   | otherwise    -> fmap concat  (mapM (\x -> objectIndex x (JObject dict)) indices)
    _                             -> Left "IteratorObj only works with Objects"


arrayIndex :: Int -> JProgram[JSON]
arrayIndex index inp = case inp of
    (JArray a) | length a > index -> if index >= 0 then Right [a !! index] else Right [a !! (length a  + index)]
               | otherwise -> Right [JNull]
    JNull ->Right [JNull]
    _ -> Left "An Array has to be provided"

objectIndex :: String -> JProgram[JSON]
objectIndex s inp = case inp of
    (JObject dict) -> let l = dropWhile (\(name, _) -> name /= s) (reverse dict)
        in if null l then Right[JNull] else Right [snd $ head l]
    JNull -> Right [JNull]
    _ -> Left "An Object has to be provided"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run f j = f j
