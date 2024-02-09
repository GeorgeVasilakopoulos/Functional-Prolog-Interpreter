import System.IO
import Types
import Lexer
import Parser
import Converter
import Matcher
import TopDownSolver


prettyPrintAssignment :: Replacement -> String
prettyPrintAssignment (term1,term2) = (show term1) ++ " <- " ++ (show term2) ++ " "


moreSolutions :: [Clause] -> (Int,Assignment,[(Pred,Assignment,[Clause])]) -> IO()
moreSolutions program (counter,assignment,state) = do
    putStr "> "
    ans <- getLine
    if (ans == "") then
        case nextsolution program (counter,assignment,state) of
            Nothing -> do
                putStrLn "No more solutions!"
            Just (counter,assignment,state) -> do
                mapM_ putStr (map prettyPrintAssignment (keepOnly 0 assignment))
                moreSolutions program (counter,assignment,state)
    else if (ans == "n") then
        querying program
    else do
        putStrLn "Invalid Option! Press 'n' to enter another query."
        moreSolutions program (counter,assignment,state)




querying:: [Clause] -> IO()
querying program = do  
    putStr "> "
    line <- getLine
    if (line == "exit") then 
        return ()
    else 
        let query = fst  (head (map convert (parser (lexer line))))
            answer = findsolution program 1 [query] [] 
        in
            case answer of
                Nothing -> do
                    putStrLn "No solution!"
                Just (counter,assignment,state) -> do
                    mapM_ putStr (map prettyPrintAssignment (keepOnly 0 assignment))
                    putStrLn "More solutions ? [y/n]"
                    moreSolutions program (counter,assignment,state)



main :: IO ()
main = do
    putStrLn "Load a Prolog file:"
    filename <- getLine
    contents <- readFile filename
    let program = map convert (parser (lexer contents))
    putStrLn "Program loaded successfully!"
    querying program