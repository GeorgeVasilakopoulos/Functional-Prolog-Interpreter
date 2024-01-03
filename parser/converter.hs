import Types
import Lexer
import Parser




convertItem :: ASTNode -> Term
convertItem (PredVariable name) = Variable name
convertItem (Predicate name []) = Atom name
convertItem (Predicate name list) = Func name (map convertItem list)

convertPred :: ASTNode -> Pred
convertPred (Predicate name list) = (name, map convertItem list)

makeClause :: ASTNode -> Clause
makeClause (Fact (Predicate name list)) = CFact (name, map convertItem list)
makeClause (Rule (Predicate name list) list2) = CRule (name, map convertItem list) (map convertPred list2) 



testme :: String -> [Clause]
testme(input) = map makeClause (parser (lexer input))






