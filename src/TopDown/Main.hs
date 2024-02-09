import System.IO
import Control.Exception (evaluate)
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
    hFlush stdout
    ans <- getLine
    if (ans == "") then
        case nextsolution program (counter,assignment,state) of
            Nothing -> do
                putStrLn "No more solutions!"
                hFlush stdout
                querying program
            Just (counter,assignment,state) -> do
                mapM_ putStr (map prettyPrintAssignment (keepOnly 0 assignment))
                putStrLn ""
                hFlush stdout
                moreSolutions program (counter,assignment,state)
    else if (ans == "n") then
        querying program
    else do
        putStrLn "Invalid Option! Press 'n' to enter another query."
        hFlush stdout
        moreSolutions program (counter,assignment,state)




querying:: [Clause] -> IO()
querying program = do  
    putStr "> "
    hFlush stdout
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
                    hFlush stdout
                Just (counter,assignment,state) -> do
                    if((length assignment) == 0) then
                        putStrLn "True"
                    else do
                        mapM_ putStr (map prettyPrintAssignment (keepOnly 0 assignment))
                        putStrLn ""
                        hFlush stdout
                    moreSolutions program (counter,assignment,state)



main :: IO ()
main = do
    putStr "Load a Prolog file: "
    hFlush stdout
    filename <- getLine
    contents <- readFile filename
    let program = map convert (parser (lexer contents))
    evaluate program
    putStrLn "Program loaded successfully!"
    querying program