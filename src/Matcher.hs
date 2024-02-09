module Matcher where

import Types
import Debug.Trace

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:l) = if(f x) then Just x else find f l




applyAssignment :: Assignment -> Term -> Term
applyAssignment _ (Atom name)  = Atom name

applyAssignment assignment (Variable name n) = 
    case find (\x -> fst x == (Variable name n)) assignment of
        Nothing -> (Variable name n)
        Just tuple -> snd tuple

applyAssignment assignment (Func name termlist) = 
    Func name $ map (applyAssignment assignment) termlist



applyAssignmentPred :: Assignment -> Pred -> Pred
applyAssignmentPred assignment (name,termlist) =
    (name, map (applyAssignment assignment) termlist)


filterAssignments :: Replacement -> Bool
filterAssignments (Variable name num, term) = Variable name num == term  




-- combineAssignments :: [String] -> Assignment -> Assignment
-- combineAssignments variablelist [] = []
-- combineAssignments variablelist (y:assignment2) =  
--     if(member (snd y) variablelist) then combineAssignments variablelist assignment2 
--     else y:(combineAssignments variablelist assignment2)


-- Compose Assignments where all variables of assignment1 are expressed in terms of variables of assignment2
composeAssignments :: Assignment -> Assignment -> Assignment
composeAssignments assignment1 assignment2 =
    (map (\(x,y) -> (x , applyAssignment assignment2 y)) assignment1)  ++ assignment2


    -- filter filterAssignments (map (\(x,y)-> ((applyAssignment assignment2 x),y)) assignment1) ++ (combineAssignmentLists (map snd assignment1) assignment2) 



-- member :: a -> [a] -> Bool
member x [] = False
member x (y:itemlist) = if(x == y) then True else member x itemlist 




isInTerm :: Term -> Term -> Bool
isInTerm (Variable name num) term 
    = case term of
        Func name2 termList ->
            isInTermList (Variable name num) termList
        Atom name ->
            False
        Variable name2 num2 ->
            (name == name2) && (num == num2) 





isInTermList :: Term -> [Term] -> Bool
isInTermList (Variable name num) [] = False
isInTermList (Variable name num) (term:termList) =
    if(isInTerm (Variable name num) term) then True
    else isInTermList (Variable name num) termList

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just a) = True



matchTermList :: [Term] -> [Term] -> Assignment -> Maybe Assignment
matchTermList [] [] assignment = Just assignment


matchTermList (term1:termlist1) (term2:termlist2) assignment =
    matchTerm term1 term2 >>= \assignment2 ->
    let new_termlist1 = map (applyAssignment assignment2) termlist1 
        new_termlist2 = map (applyAssignment assignment2) termlist2
        new_assignment = (composeAssignments assignment assignment2) 
    in 
      matchTermList new_termlist1 new_termlist2 new_assignment

    -- matchTecomposeAssignmentsrm (wholeTerm1 !! i) (wholeTerm2 !! i) >>= \assignment2 ->
    -- if (length assignment2 == 1) then 
    --     let w = applyAssignment assignment2 wholeTerm2 in
    --     if w == wholeTerm2 then 
    --         matchTermList (applyAssignment assignment2 wholeTerm1) wholeTerm2 (i+1) n (assignment ++ assignment2)
    --     else
    --         mathTermList (applyAssignment assignment2 wholeTerm1) w (i+1) n (composeAssignments assignment assignment2)
    -- else    
    --     matchTermList (applyAssignment assignment2 wholeTerm1) wholeTerm2 (i+1) n (assignment ++ assignment2)




matchTerm :: Term -> Term -> Maybe Assignment
matchTerm (Variable name1 num1) (Variable name2 num2) = 
    if ((Variable name1 num1) == (Variable name2 num2)) then Just []
    else if (num1 < num2) then Just [(Variable name1 num1,Variable name2 num2)]
    else Just [(Variable name2 num2,Variable name1 num1)]


matchTerm (Variable name num) term = 
    if(isInTerm (Variable name num) term) then  Nothing
    else Just [(Variable name num,term)]

matchTerm t1 (Variable name num) = matchTerm (Variable name num) t1

matchTerm (Atom str1) (Atom str2)
    = if(str1 == str2) then Just [] else Nothing



matchTerm (Func name1 []) (Func name2 []) = if(name1 == name2) then Just [] else Nothing

matchTerm (Func name1 termlist1) (Func name2 termlist2) = 
    if (name1 /= name2 || (length termlist1) /= (length termlist2)) then Nothing
    else matchTermList termlist1 termlist2 [] 

matchTerm _ _ = Nothing

-- matchTerm (Func str1 []) (Func str2 []) = if(str1 == str2) then Just [] else Nothing
-- matchTerm (Func str1 termlist1) (Func str2 termlist2) =
--     if ((str1 /= str2) || (length termlist1) /= (length termlist2)) then Nothing
--     else
--         matchTerm (head termlist1) (head termlist2) >>= \assignment ->
--         matchTerm (applyAssignment assignment (Func str1 (tail termlist1))) (applyAssignment assignment (Func str2 (tail termlist2))) >>= \assignment2 ->
--         Just (composeAssignments assignment assignment2)



        -- let a = matchTerm (head termlist1) (head termlist2) in
        -- case a of
        --     Nothing -> Nothing
        --     Just assignment -> -- matchTerm (applyAssignment assignment (Func str1 (tail termlist1))) (applyAssignment assignment (Func str2 (tail termlist2)))
        --         let r1 = applyAssignment assignment (Func str1 (tail termlist1))
        --             r2 = applyAssignment assignment (Func str2 (tail termlist2))
        --         in
        --             case matchTerm r1 r2 of
        --                 Nothing -> Nothing
        --                 Just assignment2 -> Just (composeAssignments assignment assignment2) 

-- matchTermList :: [Term] -> [Term] -> Maybe [Assignment]
-- matchTermList [] [] = Just []
-- matchTermList list [] = Nothing
-- matchTermList [] list = Nothing
-- matchTermList (term1:l1) (term2:l2) =
--     matchTerm term1 term2 >>= \assignment ->
--     matchTermList (map (applyAssignment assignment) l1) (map (applyAssignment assignment) l2) >>= \assignment2 ->
--     Just (composeAssignments assignment assignment2)


matchPred :: Pred -> Pred -> Maybe Assignment
matchPred (relation1, l1) (relation2, l2) = 
    if (relation1 /= relation2 || (length l1) /= (length l2)) then Nothing
    else matchTermList l1 l2 []


-- matchPredList :: [Pred] -> [Pred] -> Maybe [Assignment]
-- matchPredList [] [] = Just []
-- matchPredList l [] = Nothing
-- matchPredList [] l = Nothing
-- matchPredList l1 l2 = 
--     matchPred (head l1) (head l2) >>= \assignment1 ->
--     matchPredList (map (applyAssignmentPred assignment1) $ tail l1) (map (applyAssignmentPred assignment1) $ tail l2) >>= \assignment2 ->
--     Just (composeAssignments assignment1 assignment2)





-- matchPredList :: [Pred] -> [Pred] -> Maybe [Assignment]
-- matchPredList predlist1 predlist2 = 
--     if((length predlist1) /= (length predlist2)) then Nothing
--     else

keepOnly :: Int -> Assignment -> Assignment
keepOnly i = filter (\(Variable name n,_) -> (n == i))



