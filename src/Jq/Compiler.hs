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
compile (Slice l u) inp =  case inp of
    (JArray arr) | length arr < lower  -> Right []
                 | length arr < upper -> Right [JArray (take (length arr - lower) (drop lower arr))]
                 | otherwise -> Right [JArray (take (upper - lower) (drop lower arr))]
        where
            lower = convert l (length arr)
            upper = convert u (length arr)
    (JString s)  | length s < lower  -> Right []
                 | length s < upper -> Right [JString (take (length s - lower) (drop lower s))]
                 | otherwise -> Right [JString (take (upper - lower) (drop lower s))]
        where
            lower = convert l (length s)
            upper = convert u (length s)
    JNull ->Right [JNull]
    _ -> Left "An Array has to be provided"
compile (Iterator indices) inp = case inp of
    (JArray arr)   | null indices -> Right arr
                   | otherwise    -> fmap concat  (mapM (\x -> arrayIndex x (JArray arr)) indices)
    (JObject dict) | null indices -> Right (map snd dict)
                   | otherwise    -> Left "asd"
    JNull -> Right (map (const JNull) indices)
    _                             -> Left "Iterator only works with Arrays"
compile (IteratorObj indices) inp = case inp of
    (JObject dict) | null indices -> Right (map snd dict)
                   | otherwise    -> fmap concat  (mapM (\x -> objectIndex x (JObject dict)) indices)
    JNull -> Right (map (const JNull) indices)
    _                             -> Left "IteratorObj only works with Objects"
compile RecursiveDescent inp = Right (JString <$> recursive inp)

recursive :: JSON -> [String]
recursive inp = case inp of
    (JObject xs) -> show inp : concatMap (recursive . snd) xs
    (JArray xs) -> show inp : concatMap recursive xs
    _ -> [show inp]


convert :: Int -> Int -> Int
convert bound l = if bound >= 0 then bound else l + bound

arrayIndex :: Int -> JProgram[JSON]
arrayIndex i inp = case inp of
    (JArray a) | length a > abs i -> Right [a !! index]
               | otherwise -> Right [JNull]
        where index = convert i (length a)
    JNull ->Right [JNull]
    _ -> Left "An Array has to be provided"

objectIndex :: String -> JProgram[JSON]
objectIndex s inp = case inp of
    (JObject dict) -> let l = dropWhile (\(name, _) -> name /= s) (reverse dict)
        in if null l then Right [JNull] else Right [snd $ head l]
    JNull -> Right [JNull]
    _ -> Left "An Object has to be provided"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run f j = f j
