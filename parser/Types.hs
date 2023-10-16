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





find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:l) = if(f x) then Just x else find f l




applyAssignment :: [Assignment] -> Term -> Term
applyAssignment assignment (Atom name)  = Atom name


applyAssignment assignment (Variable name) 
    = case find (\x -> snd x == name) assignment of
        Nothing ->
            Variable name
        Just tuple ->
            fst tuple

applyAssignment assignment (Func name termlist)  = 
    Func name (map (applyAssignment assignment) termlist)


filterAssignments (term,x) = case term of 
    Variable name -> (name == x)
    _ -> False 




combineAssignmentLists :: [String] -> [Assignment] -> [Assignment]
combineAssignmentLists variablelist [] = []
combineAssignmentLists variablelist (y:assignment2) =  
    if(member (snd y) variablelist) then combineAssignmentLists variablelist assignment2 
    else y:(combineAssignmentLists variablelist assignment2)


composeAssignments :: [Assignment] -> [Assignment] -> [Assignment]
composeAssignments assignment1 assignment2 =
    filter filterAssignments (map (\(x,y)-> ((applyAssignment assignment2 x),y)) assignment1) ++ (combineAssignmentLists (map snd assignment1) assignment2) 



-- member :: a -> [a] -> Bool
member x [] = False
member x (y:itemlist) = if(x == y) then True else member x itemlist 




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





-- matchTerm :: Term -> Term -> Maybe [Assignment]

-- matchTerm (Variable str1) (Variable str2) = 
--     if(str1 == str2) then Just []
--     else Just [(Variable str1,str2)]

-- matchTerm (Variable str) term = 
--     if(isInTerm str term) then Nothing
--     else Just [(term,str)]

-- matchTerm t1 (Variable str1) = matchTerm (Variable str1) t1

-- matchTerm (Atom str1) (Atom str2)
--     = if(str1 == str2) then Just [] else Nothing


-- matchTerm (Func str1 termlist1) (Func str2 termlist2) =



-- matchQuery ::  Pred -> Pred -> Bool
-- matchQuery p1 p2 =  
