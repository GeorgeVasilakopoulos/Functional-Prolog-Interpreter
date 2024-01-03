import Types



topDown :: [Clause] -> Pred -> Bool
topDown (clause:clauses) query
    case clause of
        CFact pred -> if matchPred query pred then True else topDown query clauses
        CRule pred preds -> if matchPred query pred then all (topDown query) preds else topDown query clauses

