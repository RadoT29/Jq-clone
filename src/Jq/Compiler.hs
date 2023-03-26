module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Either (lefts, rights)

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (Parenthesis a) inp = compile a inp
compile (ObjectIndex s) inp = objectIndex s inp
compile (Pipe a b) inp = compile a inp >>= fmap concat . mapM (compile b)
compile (Comma a b) inp = compile a inp >>= (\x -> fmap (x ++)  (compile b inp))
compile (Optional f) inp = let output = compile f inp
    in case output of
        (Left _) -> return []
        right    -> right
compile (ArrayIndex index) inp = arrayIndex index inp
compile (Slice l u) inp =  case inp of
    (JArray arr) -> Right [JArray $ applySlice arr l u]
    (JString s)  -> Right [JString $ applySlice s l u]
    JNull        -> Right [JNull]
    _            -> Left "An Array/String has to be provided"
compile (Iterator indices) inp = case inp of
    (JArray _)  | null (rights indices) -> concat <$> mapM (`arrayIndex` inp) (lefts indices)
                | otherwise             -> Left "Arrays can only be indexed with numbers in Iterator"
    (JObject _) | null (lefts indices)  -> concat <$> mapM (`objectIndex` inp) (rights indices)
                | otherwise             -> Left "Objects can only be indexed with strings in Iterator"
    JNull                               -> Right (map (const JNull) indices)
    _                                   -> Left "Iterator only works with Arrays and Objects"
compile EmptyIteration inp = case inp of
    (JObject dict) -> Right (map snd dict)
    (JArray arr)   -> Right arr
    other          -> Left $ "Itearation doesn't work with: " ++ show other
compile RecursiveDescent inp = Right (recursive inp)
compile (Jval v) _ = return [v]
compile (ArrConst arr) inp = case concat <$>  mapM (`compile` inp) arr  of
    (Left v) -> Left v
    (Right array)    -> Right [JArray array]
compile (ObjConst dict) inp =  case l of
    (Left _)       -> l
    (Right values) -> Right [JObject (zip (map fst dict) values)]
    where l = concat <$> mapM ((`compile` inp) . snd) dict
compile (Try t c) inp = case compile t inp of
    (Left _)  -> compile c inp
    right     -> right
compile (Plus a b) inp = case (compile a inp, compile b inp) of
    (Right n, Right m) -> sequence $ plusMapping <$> n <*> m
    (Left n, _) -> Left n
    (_, Left m) -> Left m
compile (Minus a b) inp = case (compile a inp, compile b inp) of
    (Right n, Right m) -> sequence $ minusMapping <$> n <*> m
    (Left n, _) -> Left n
    (_, Left m) -> Left m
compile (Mult a b) inp = case (compile a inp, compile b inp) of
    (Right n, Right m) -> sequence $ multMapping <$> n <*> m
    (Left n, _) -> Left n
    (_, Left m) -> Left m
compile (Div a b) inp = case (compile a inp, compile b inp) of
    (Right n, Right m) -> sequence $ divMapping <$> n <*> m
    (Left n, _) -> Left n
    (_, Left m) -> Left m
-- compile (Equiv a b) inp = case (compile a inp, compile b inp) of
--     (Left a, _) -> Left a
--     (_, Left a) -> Left a
--     (a, b)      -> Right 

plusMapping :: JSON -> JSON -> Either String JSON
plusMapping (JNumber n) (JNumber m) = Right $ JNumber $ n + m
plusMapping (JArray n)  (JArray m)  = Right $ JArray $ n ++ m
plusMapping (JString n) (JString m) = Right $ JString $ n ++ m
plusMapping (JObject n) (JObject m) = Right $ JObject $ n ++ m
plusMapping x JNull = Right x
plusMapping JNull x = Right x
plusMapping _ _ = Left "Incorrect addition"

minusMapping :: JSON -> JSON -> Either String JSON
minusMapping (JNumber n) (JNumber m) = Right $ JNumber $ n - m
minusMapping (JArray n) (JArray m) = Right $ JArray $ deleteItem n  m
minusMapping _ _ = Left "Incorrect subtraction"

multMapping :: JSON -> JSON -> Either String JSON
multMapping (JNumber n) (JNumber m) = Right $ JNumber $ n * m
multMapping (JString n) (JNumber m) = if m == 0 then Right JNull else Right $ JString (concatMap (const n) [0..m])
multMapping (JNumber m) (JString n) = if m == 0 then Right JNull else Right $ JString (concatMap (const n) [0..m])
multMapping (JObject n) (JObject m) = Right $ JObject (newN ++ restM)
        where newN = handleObjMult n (filter (\x -> fst x `elem` map fst n) m)
              restM = filter (\x -> fst x `notElem` map fst n) m
multMapping _ _ = Left "Incorrect multiplication"

divMapping :: JSON -> JSON -> Either String JSON
divMapping (JNumber n) (JNumber m) = if m == 0 then Left "Div by 0" else Right $ JNumber $ n / m
divMapping (JString n) (JString m) = Right $ JArray $ map JString (splitOn m n 0)
divMapping _ _ = Left "Incorrect division"

handleObjMult :: [(String, JSON)] -> [(String, JSON)] -> [(String, JSON)]
handleObjMult [] _ = []
handleObjMult (x:xs) ys = if fst x `elem` map fst ys then case (snd x,snd $ head $ dropWhile (\y -> fst y /= fst x) ys) of
    (JObject n, JObject m) -> (fst x, JObject $ handleObjMult n m ++ filter (\y -> fst y `notElem` map fst n) m) : handleObjMult xs ys
    (_, m)                     -> (fst x, m) : handleObjMult xs ys
    else x : handleObjMult xs ys

deleteItem :: [JSON] -> [JSON] -> [JSON]
deleteItem [] _ = []
deleteItem (x:xs) ys = if x `elem` ys then deleteItem xs ys else x : deleteItem xs ys

splitOn :: String -> String -> Int -> [String]
splitOn _ "" _ = []
splitOn d s c | d == applySlice s c (c + length d) = applySlice s 0 c : splitOn d (applySlice s (c + length d) (length s)) 0
              | c == length s                  = [s]
              | otherwise                      = splitOn d s (c + 1)

recursive :: JSON -> [JSON]
recursive inp = case inp of
    (JObject xs) -> inp : concatMap (recursive . snd) xs
    (JArray xs)  -> inp : concatMap recursive xs
    x -> [x]


applySlice :: [a] -> Int -> Int -> [a]
applySlice xs l u =
    if lower < upper then
        let
            left = max 0 lower
            right = min (length xs) upper
        in take (right - left) (drop left xs)
    else []

    where
        lower = convert l (length xs)
        upper = convert u (length xs)

convert :: Int -> Int -> Int
convert bound l = if bound >= 0 then bound else l + bound

arrayIndex :: Int -> JProgram[JSON]
arrayIndex i inp = case inp of
    (JArray a) | length a > index && index >= 0 -> Right [a !! index]
               | otherwise                      -> Right [JNull]
        where index = convert i (length a)
    JNull                                       ->Right [JNull]
    _                                           -> Left "An Array has to be provided"

objectIndex :: String -> JProgram[JSON]
objectIndex s inp = case inp of
    (JObject dict) -> let l = dropWhile (\(name, _) -> name /= s) dict
        in if null l then Right [JNull] else Right [snd $ head l]
    JNull          -> Right [JNull]
    _              -> Left "An Object has to be provided"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run f = f
