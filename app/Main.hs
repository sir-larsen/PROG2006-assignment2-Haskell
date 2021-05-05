module Main where

import Interpreter
import Data.Char(toUpper)

-- The loop for REPL mode
loop :: Stack -> IO()
loop stack = do
    input <- getLine
    let newStack = parse (words input) stack
    printStack newStack
    loop newStack

main :: IO ()
main = do
    -- Takes input from user
    input <- getLine
    -- Building the initial stack from the first input
    let stack = parse (words input) []
    --Printing the stack
    printStack stack
    -- Loops our program until the user shuts it down
    loop stack
