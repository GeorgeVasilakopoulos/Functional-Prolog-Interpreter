module TopDownSolver (findsolution,nextsolution) where

import Types
import Matcher
import Debug.Trace

setVariableCounterInTerm :: Int -> Term -> Term
setVariableCounterInTerm count term = 
    case term of
       Variable name num -> Variable name (count)
       Atom name -> Atom name
       Func name termlist -> Func name (map (setVariableCounterInTerm count) termlist)




setVariableCounters :: Int -> Clause -> Clause
setVariableCounters counter ((name,terms),preds) = 
    (
       (name, map (setVariableCounterInTerm counter) terms), 
       (map (\(n,l)->(n,map (setVariableCounterInTerm counter) l)) preds)
    )




firstMatchingRule :: [Clause] -> Pred -> Maybe (Assignment, [Clause])
firstMatchingRule [] _ = Nothing
firstMatchingRule (rule:program) query = 
    case matchPred query (fst rule) of
       Just assignment -> Just (assignment,rule:program) 
       Nothing -> firstMatchingRule program query



findsolution :: [Clause] -> Int -> [Pred] -> Assignment -> Maybe (Int,Assignment,[(Pred,Assignment,[Clause])])
findsolution _ counter [] assignment = Just (counter,assignment,[])
findsolution program counter (q:stack) assignment = 
    let program' = map (setVariableCounters counter) program in 
    
    (firstMatchingRule program' q) >>= \(assignment',rule:rest) ->

    let assignment'' = composeAssignments assignment assignment'
        new_queries = map (applyAssignmentPred assignment') (snd rule)
        stack' = new_queries ++ (map (applyAssignmentPred assignment') stack)
    in
        findsolution program' (counter+1) stack' assignment'' >>= \(counter',assignment''',state)->
            Just (counter',assignment''',(q,assignment,rest):state)




nextsolution :: [Clause] -> (Int,Assignment,[(Pred,Assignment,[Clause])]) -> Maybe (Int,Assignment,[(Pred,Assignment,[Clause])])
nextsolution _ (counter,_,[]) = Nothing
nextsolution original_program (counter,_,((q,assignment,program):state)) = 
    case nextsolution original_program (counter,assignment,state) of 
        Just (counter',assignment',state') ->
            Just (counter',assignment',(q,assignment,program):state')
        Nothing ->
            let program' = map (setVariableCounters (counter + 1)) program in

            (firstMatchingRule program' q) >>= \(assignment', rule:rest) ->

            let assignment'' = composeAssignments assignment assignment'
                new_queries = map (applyAssignmentPred assignment') (snd rule)
                stack' = new_queries
            in
                findsolution original_program (counter+2) stack' assignment'' >>= \(counter',assignment''',state)->
                    Just (counter',assignment''',(q,assignment,rest):state)





