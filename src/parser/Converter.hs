module Converter (convert) where


import Types
import Lexer
import Parser
import Matcher
-- import Naive
-- import TopDown


convertItem :: ASTNode -> Term
convertItem (PredVariable name) = Variable name 0
convertItem (Predicate name []) = Atom name
convertItem (Predicate name list) = Func name (map convertItem list)

convertPred :: ASTNode -> Pred
convertPred (Predicate name list) = (name, map convertItem list)

convert :: ASTNode -> Clause
convert (Fact (Predicate name list)) = ((name, map convertItem list),[]) 
convert (Rule (Predicate name list) list2) = ((name, map convertItem list),(map convertPred list2)) 



-- basic testing



-- testme :: String -> String -> Maybe Assignment
-- testme str1 str2 =  let w1 = map makeClause (parser (lexer str1))
--                         w2 = map makeClause (parser (lexer str2))
--                     in 
--                         case (w1,w2) of
--                             ([CFact p1], [CFact p2]) -> (matchPred p1 p2)
--                             _ -> Just []








