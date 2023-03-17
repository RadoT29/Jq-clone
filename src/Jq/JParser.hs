module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

-- parseNum :: Parser JSON
-- parseNum =  

-- parseJString :: Parrser JSON
-- parseJString = do 

parseJSON :: Parser JSON
parseJSON = token $ parseJNull
