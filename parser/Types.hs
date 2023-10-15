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





data ASTNode
    =   Fact Pred
    |   Rule Pred [Pred]
    -- |   Predicate Relation
    -- |   Relation String
    -- |   Term String
    -- |   Variable String
    deriving (Eq, Show)


type Pred = (Relation, [Term]) 

type Relation = String --LowerStr

data Term 
    = Variable String
    | Atom String
    | Func String [Term]
    deriving (Eq, Show)

type Assignment = (Term,String)




isInTerm :: String -> Term -> Bool
isInTerm variable term 
    = case term of
        Func name termList ->
            isInTermList variable termList
        Atom name ->
            False
        Variable name ->
            variable == name





isInTermList :: String -> [Term] -> Bool
isInTermList variable [] = False
isInTermList variable (term:termList) =
    if(isInTerm variable term) then True
    else isInTermList variable termList





matchTerm :: Term -> Term -> Maybe [Assignment]

matchTerm (Variable str1) (Variable str2) = 
    if(str1 == str2) then Just []
    else Just [(Variable str1,str2)]

matchTerm (Variable str) term = 
    if(isInTerm str term) then Nothing
    else Just [(term,str)]

matchTerm t1 (Variable str1) = matchTerm (Variable str1) t1

matchTerm (Atom str1) (Atom str2)
    = if(str1 == str2) then Just [] else Nothing


-- matchQuery ::  Pred -> Pred -> Bool
-- matchQuery p1 p2 =  






