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
    (JArray arr) -> Right [JArray $ applySlice arr l u] 
    (JString s) -> Right [JString $ applySlice s l u]
    JNull ->Right [JNull]
    _ -> Left "An Array/String has to be provided"
compile (Iterator indices) inp = case inp of
    (JArray arr) -> fmap concat  (mapM (\x -> arrayIndex x (JArray arr)) indices)
    JNull -> Right (map (const JNull) indices)
    _                             -> Left "Iterator only works with Arrays"
compile (IteratorObj indices) inp = case inp of
    (JObject dict) -> fmap concat  (mapM (\x -> objectIndex x (JObject dict)) indices)
    JNull -> Right (map (const JNull) indices)
    _                             -> Left "IteratorObj only works with Objects"
compile EmptyIteration inp = case inp of
      (JObject dict) -> Right (map snd dict)
      (JArray arr) -> Right arr
      _ -> Left "Itearation doesn't work with this type"
compile RecursiveDescent inp = Right (recursive inp)

recursive :: JSON -> [JSON]
recursive inp = case inp of
    (JObject xs) -> inp : concatMap (recursive . snd) xs
    (JArray xs) -> inp : concatMap recursive xs
    x -> [x]


applySlice :: [a] -> Int -> Int -> [a]
applySlice xs l u =
    if lower < upper then
        take  (min (length xs) upper) (drop (max 0 lower) xs)
    else []

    where
        lower = convert l (length xs)
        upper = convert u (length xs)

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
