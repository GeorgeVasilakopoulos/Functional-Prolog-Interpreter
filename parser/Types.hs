module Types where

data Token 
    =   LexerError
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

{-
data ASTNode
    =   Fact
    |   Rule
    |   Predicate
    |   Relation String
    |   Term String
    |   Variable String
    deriving (Eq, Show)
-}  
