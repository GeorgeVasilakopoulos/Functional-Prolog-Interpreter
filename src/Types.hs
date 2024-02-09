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




type Clause = (Pred,[Pred])


showlist :: Show a => [a] -> String
showlist [] = ""
showlist [x] = show x
showlist (x:xs) = (show x) ++ "," ++ (showlist xs)



type Pred = (String, [Term]) 


data Term 
    = Variable String Int
    | Atom String
    | Func String [Term]
    deriving (Eq)

instance Show Term where
    show (Variable name num) = name -- ++ "_" ++ (show num)
    show (Atom name) = name
    show (Func name termlist) = name ++ "(" ++ (showlist termlist) ++ ")"




type Replacement = (Term,Term)

type Assignment = [Replacement]



