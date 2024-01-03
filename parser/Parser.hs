module Parser (parser) where

import Types
import Lexer (lexer)


{-

The Parser is responsible for accepting the tokens produced by the lexer and returning an Abstract Syntax Tree (AST) of the provided CFG.

-}
  
consume :: Token -> [Token] -> [Token]
consume x (l:ls)
    | x == l = ls
    | otherwise = [LexerError]
    
    
    
-- STATEMENT
-- Note: the '.' character is removed by the lexer
-- FIRST+ = {LowerStr}
stmt :: [Token] -> ASTNode
-- Stmt -> Predicate StmtTail '.'
stmt (LowerStr str:line) = do
    let (headLine, headAST) = predicate (LowerStr str:line)
    if headLine == [] then
        Fact headAST
    else
        Rule headAST (stmtTail headLine)

stmt x = ParserError

    
    
-- PREDICATE
-- FIRST+ = {LowerStr}
predicate :: [Token] -> ([Token], ASTNode)
-- Predicate -> LowerStr Pred2
predicate (LowerStr str:line) = do
    let (pred2Line, pred2AST) = pred2 line
    (pred2Line, Predicate str pred2AST)

predicate (x:xs) = (xs, Debug x)

-- FIRST+ = {LParen, RParen, Comma, $}
pred2 :: [Token] -> ([Token], [ASTNode])
-- Pred2 -> '(' TermList ')'
pred2 (LParen:line) = do
    let (termListLine, termListAST) = termList line
    let retLine = consume RParen termListLine
    (retLine, termListAST)
-- Pred2 -> e
pred2 [] = ([], [])
pred2 (RParen:line) = ((RParen:line), [])
pred2 (Comma:line) = ((Comma:line), [])

pred2 (str:line) = ((str:line), [Debug str])



-- STATEMENT TAIL
stmtTail :: [Token] -> [ASTNode]
-- StmtTail -> ':-' PredList
stmtTail (Colon:(Dash:line)) = snd (predList line)
-- StmtTail -> e
-- Covered in the 'stmt' section

stmtTail (x:xs) = [Debug x]



-- PREDICATE LIST
-- FIRST+ = {LowerStr}
predList :: [Token] -> ([Token], [ASTNode])
-- PredList -> Predicate PredList2
predList (LowerStr str:line) = do
    let (predLine, predAST) = predicate (LowerStr str:line)
    let (predList2Line, predList2AST) = predList2 predLine
    (predList2Line, (predAST:predList2AST))

-- FIRST+ = {Comma, $}
predList2 :: [Token] -> ([Token], [ASTNode])
-- PredList2 -> ',' Predicate PredList2
predList2 (Comma:(LowerStr str:line)) = do
    let (predLine, predAST) = predicate (LowerStr str:line)
    let (predList2Line, predList2AST) = predList2 predLine
    (predList2Line, (predAST:predList2AST))
-- PredList2 -> e
predList2 [] = ([], [])


    
-- TERM LIST
-- FIRST+ = {LowerStr, UpperStr, Number, $}
termList :: [Token] -> ([Token], [ASTNode])
-- TermList -> Term TermList2
termList (LowerStr str:line) = do
    let (predLine, predAST) = predicate (LowerStr str:line)
    let (termList2Line, termList2AST) = termList2 predLine
    (termList2Line, (predAST:termList2AST))
termList (UpperStr str:line) = do
    let (termList2Line, termList2AST) = termList2 line
    let predVar = PredVariable str
    (termList2Line, (predVar:termList2AST))
termList (Number num:line) = do
    let (termList2Line, termList2AST) = termList2 line
    let predNum = Predicate (show num) []
    (termList2Line, (predNum:termList2AST))

-- FIRST+ = {Comma, RParen}
termList2 :: [Token] -> ([Token], [ASTNode])
-- TermList2 -> ',' Term TermList2
termList2 (Comma:(LowerStr str:line)) = do
    let (predLine, predAST) = predicate (LowerStr str:line)
    let (termList2Line, termList2AST) = termList2 predLine
    (termList2Line, (predAST:termList2AST))
termList2 (Comma:(UpperStr str:line)) = do
    let (termList2Line, termList2AST) = termList2 line
    let predVar = PredVariable str
    (termList2Line, (predVar:termList2AST))
termList2 (Comma:(Number num:line)) = do
    let (termList2Line, termList2AST) = termList2 line
    let predNum = Predicate (show num) []
    (termList2Line, (predNum:termList2AST))
-- TermList2 -> e
termList2 (RParen:line) = ((RParen:line), [])
termList2 x = ([], [ParserError])




parser :: [[Token]] -> [ASTNode]
parser [] = []
parser ([]:input) = parser(input)
parser (line:input) = stmt(line):parser(input)



