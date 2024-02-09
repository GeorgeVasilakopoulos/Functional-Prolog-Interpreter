import Types
import Matcher



topDown :: [Clause] -> Pred -> Pred
topDown clauses pred = topDownHelper clauses clauses pred

topDownHelper :: [Clause] -> [Clause] -> Pred -> Maybe Pred
topDownHelper _ [] _ = Nothing
topDownHelper allClauses (clause:clauses) pred =
    case clause of
        CFact fact ->
            case matchPred fact pred of
                Just assignment -> Just $ applyAssignmentPred assignment pred
                Nothing -> topDownHelper allClauses clauses pred
        CRule ruleHead ruleTail -> 
            case  matchPred ruleHead pred of
                Just assignment -> 
                    let newPred = applyAssignmentPred assignment pred in
                    case matchPredToList newPred ruleTail of
                        Just assignment2 -> Just $ applyAssignmentPred assignment2 newPred
                        Nothing -> topDownHelper allClauses clauses pred
                    
                Nothing -> topDownHelper allClauses clauses pred


matchPredToList :: Pred -> [Pred] -> Maybe [Assignment]
matchPredToList pred [] = Just []
matchPredToList pred (p:ps) = 
    case matchPred pred p of
        Nothing -> Nothing
        Just assignment -> 
            let newPred = applyAssignmentPred assignment pred in
            case matchPredToList newPred ps of
                Nothing -> Nothing
                Just assignment2 -> Just (composeAssignments assignment assignment2)