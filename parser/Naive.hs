module Naive where

import Types
import Matcher
import Control.Monad (replicateM)
import Data.List (nub)
import Debug.Trace


maybeFilter :: [Maybe a] -> [a]
maybeFilter [] = []
maybeFilter ((Nothing):l) = maybeFilter l
maybeFilter ((Just x):l) = (x : (maybeFilter l))



naiveStep :: [Clause] -> [Pred] -> [Pred]
naiveStep [] c = []

naiveStep ((CFact fact):clauseList) factList = 
    (nub $ map (\x->applyAssignmentPred x fact) $ maybeFilter $ map (matchPredList [fact]) (replicateM 1 factList)) ++ (naiveStep clauseList factList)

naiveStep ((CRule newFact predList):clauseList) factList = 
    (nub $ map (\x->applyAssignmentPred x newFact) $ maybeFilter $ map (matchPredList predList)(replicateM (length predList) factList)) ++ (naiveStep clauseList factList)  

