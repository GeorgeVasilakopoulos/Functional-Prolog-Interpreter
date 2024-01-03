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
    Variable name -> (name /= x)
    _ -> True 




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

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just a) = True




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




matchTerm (Func str1 []) (Func str2 []) = if(str1 == str2) then Just [] else Nothing
matchTerm (Func str1 termlist1) (Func str2 termlist2) =
    if ((str1 /= str2) || (length termlist1) /= (length termlist2)) then Nothing
    else
        let a = matchTerm (head termlist1) (head termlist2) in
        case a of
            Nothing -> Nothing
            Just assignment -> -- matchTerm (applyAssignment assignment (Func str1 (tail termlist1))) (applyAssignment assignment (Func str2 (tail termlist2)))
                let r1 = applyAssignment assignment (Func str1 (tail termlist1))
                    r2 = applyAssignment assignment (Func str2 (tail termlist2))
                in
                    case matchTerm r1 r2 of
                        Nothing -> Nothing
                        Just assignment2 -> Just (composeAssignments assignment assignment2) 



-- matchPred :: Pred -> [Pred] -> Maybe [Assignment]





