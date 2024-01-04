module Types where

data Token 
    =   LexerError
    |   LexerDebug String
    |   LParen
    |   RParen
    |   Colon
    |   Dash
    |   Comma
    |   Dot
    |   LowerStr String     -- [a-z] [a-z, A-Z, 0-9]*
    |   UpperStr String     -- [A-Z] [a-z, A-Z, 0-9]*
    |   Number Int          -- [0-9]+
    deriving (Eq, Show)


data ASTNode
    =   ParserError
    |   Debug Token
    |   Fact ASTNode
    |   Rule ASTNode [ASTNode]
    |   Predicate String [ASTNode]
    |   PredVariable String
    deriving (Eq, Show)




data Clause
    =   CFact Pred
    |   CRule Pred [Pred]
    deriving (Eq)

instance Show Clause where
    show (CFact pred) = show pred ++ "."
    show (CRule pred predlist) = show pred ++ " :- " ++ (showList predlist) ++ "."

showList :: [a] -> String
showList [] = ""
showList [x] = show x
showList (x:xs) = (show x) ++ "," ++ (showList xs)





type Pred = (Relation, [Term]) 

type Relation = String --LowerStr




data Term 
    = Variable String
    | Atom String
    | Func String [Term]
    deriving (Eq)

instance Show Term where
    show (Variable name) = name
    show (Atom name) = name
    show (Func name termlist) = name ++ "(" ++ (showList termlist) ++ ")"


type Assignment = (Term,String)



-- f(f(x,y),g(f(z,z),y))
-- f(f(x,y),g(f(z,x),y))

-- f(x,5)
-- f(3,x)



-- term1 = Func "f" [Func "f" [Variable "y",Variable "y"], Func "g" [Func "f" [Variable "z",Variable "z"],Variable "y"]]
-- term2 = Func "f" [Func "f" [Variable "x",Variable "y"], Func "g" [Func "f" [Atom "3",Variable "x"],Atom "1"]]



-- matchQuery ::  Pred -> Pred -> Bool
-- matchQuery p1 p2 =  
