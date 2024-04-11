module Main where

import Interpreter
import System.IO
import System.Environment

testResult :: Eq a => a -> a -> Bool
testResult expr res = expr == res

testCases :: [Bool]
testCases = 
    [
        testResult (show $ evalLambdaExpr "(λy.λx.(y y) (x x))") "λa.((x x) (x x))",
        testResult (show $ evalLambdaExpr "(λx.λy.(x y) y)") "λa.(y a)",
        testResult (show $ evalLambdaExpr "((λx.λy.λz.(x (y z)) z) y)") "λa.(z (y a))",
        testResult (show $ evalLambdaExpr "(λx.(λx.(x x) λx.x) (x x))") "λa.a",
        testResult (show $ evalLambdaExpr "(x λx.(λx.y λy.(x z)))") "(x λx.y)",
        testResult (show $ evalLambdaExpr "(λn.λf.λx.(f ((n f) x)) λf.λx.(f (f (f x))))") "λf.λx.(f (f (f (f x))))" -- (succ 3) in Church numerals
    ]

printResults :: [Bool] -> IO ()
printResults tests = printResults' tests 1
    where
        printResults' :: (Show a, Num a) => [Bool] -> a -> IO ()
        printResults' [] _ = return ()
        printResults' (x : xs) i = do
            putStrLn $ "TEST " ++ show i ++ ": " ++ show x
            printResults' xs (i + 1)

printString :: String -> IO ()
printString str = do
    putStr str
    hFlush stdout

getInput :: IO ()
getInput = do
    printString "Input the λ-expression you wish to calculate: "
    expr <- getLine
    putStrLn ("The result is: " ++ show (evalLambdaExpr expr))

main :: IO ()
main = do
    args <- getArgs
    case head args of
        "test" -> printResults testCases
        "compile" -> getInput
        _ -> undefined