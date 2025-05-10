module Main (main) where

import Interpreter
import System.IO
import Format
import Types
import System.Environment (getArgs)

-- Entry point for the interpreter
main :: IO ()
main = do
    args <- getArgs  -- Get command-line arguments
    case args of
        -- No arguments: Start REPL
        [] -> repl
        -- One argument: Interpret the file
        [filename] -> interpretFile filename
        -- Multiple arguments: Show usage
        _ -> putStrLn "Usage: bprog [filename]"

-- Interpret a file and print the result
interpretFile :: String -> IO ()
interpretFile filename = do
    contents <- readFile filename  -- Read file contents
    case bprogList contents of
        -- Handle parsing errors
        Left err -> putStrLn $ "Parse error: " ++ show err
        -- Parse successful, evaluate the program
        Right parsed -> case bprogParse parsed ([], []) of
            -- Handle evaluation errors
            Left err -> putStrLn $ "Evaluation error: " ++ show err
            -- Check stack result
            Right (_, stack) -> 
                case stack of
                    -- Empty stack error
                    [] -> putStrLn "Error: Program resulted in empty stack"
                    -- Single value: print it
                    [_] -> putStrLn $ formatResult stack
                    -- Multiple values error
                    _ -> putStrLn "Error: Program resulted in multiple values on stack"

-- Start the REPL (Read-Eval-Print Loop)
repl :: IO ()
repl = replLoop ([], [])  -- Initialize with empty environment and stack

-- REPL loop
replLoop :: EvalState -> IO ()
replLoop state@(_, stack) = do
    putStr "bprog> "  -- Prompt
    hFlush stdout
    input <- getLine  -- Read user input
    if input == "exit"
      then putStrLn "Goodbye!"  -- Exit condition
      else do
        -- Parse input
        case bprogList input of
          Left err -> putStrLn $ "Parse error: " ++ show err  -- Parse error
          Right parsed -> case bprogParse parsed state of
            -- Evaluation successful: print new stack
            Right newState@(_, newStack) -> do
              putStrLn $ formatResult newStack
              replLoop newState  -- Continue loop
            -- Evaluation error
            Left err -> do
              putStrLn $ "Error: " ++ show err
              replLoop state  -- Continue loop with previous state