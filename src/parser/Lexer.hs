module Lexer (lexer) where


import Types

{-

The Lexer is responsible for accepting the file contents and producing tokens which are used by the parser

-}

-- Utility functions
isUpper :: Char -> Bool
isUpper char = char >= 'A' && char <= 'Z'

isLower :: Char -> Bool
isLower char = char >= 'a' && char <= 'z'

isNumber :: Char -> Bool
isNumber char = char >= '0' && char <= '9'

isAlpha :: Char->Bool
isAlpha char = isUpper char || isLower char || isNumber char

strToInt :: String -> Int
strToInt str = read str


-- Recursive functions producing LowerStr, UpperStr and Number types. They work in conjunction with lexerRec
lexerCharUpper :: String -> String -> [Token]
lexerCharUpper [] aggregate = [UpperStr aggregate]
lexerCharUpper (char:input) aggregate
    | isAlpha char = lexerCharUpper input (aggregate ++ [char])
    | otherwise = (UpperStr aggregate:lexerRec (char:input))

lexerCharLower :: String -> String -> [Token]
lexerCharLower [] aggregate = [LowerStr aggregate]
lexerCharLower (char:input) aggregate
    | isAlpha char = lexerCharLower input (aggregate ++ [char])
    | otherwise = (LowerStr aggregate:lexerRec (char:input))
    
    
lexerNum :: String -> String -> [Token]
lexerNum [] aggregate = [Number (strToInt aggregate)]
lexerNum (char:input) aggregate
    | isNumber char = lexerNum input (aggregate ++ [char])
    | otherwise = (Number (strToInt aggregate):lexerRec (char:input))
    
    
-- Main recursive lexer function
lexerRec :: String -> [Token]
lexerRec [] = []
lexerRec (char:input)
    | char == '(' = (LParen:lexerRec input)
    | char == ')' = (RParen:lexerRec input)
    | char == ':' = (Colon:lexerRec input)
    | char == '-' = (Dash:lexerRec input)
    | char == ',' = (Comma:lexerRec input)
    | char == '.' = (Dot:lexerRec input)
    | char == ' ' || char == '\n' || char == '\t' = lexerRec input
    | isUpper char = lexerCharUpper (char:input) [] 
    | isLower char = lexerCharLower (char:input) []
    | isNumber char = lexerNum (char:input) []
    | otherwise = [LexerError]


-- Separates token list on Dot
splitLines :: [Token] -> [Token] -> [[Token]]
splitLines [] aggregate = [aggregate]
splitLines [token] aggregate
    | token == Dot = [aggregate]
    | otherwise = [[LexerError]]
splitLines (token:tokenList) aggregate
    | token == Dot = (aggregate:splitLines tokenList [])
    | otherwise = splitLines tokenList (aggregate ++ [token])
        

-- Exported function
lexer :: String -> [[Token]]
lexer input = splitLines (lexerRec input) []

