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



{-|main :: IO ()
--main = someFunc
--main = interact $ unlines . map processLine . lines
--main = interact (map toUpper)
main = do 
    putStrLn "test"
    --let u = Ttypes (Tint 5)
    --addStack u
    --line <- getContents
    --let u = Tlist ([Ttypes (Tint 5), Tlist ([Ttypes (Tint 6), Ttypes (Tint 7)]), Ttypes (Tint 8)]) VIRKE
    let x = Tlist [Ttypes (Tint 5), Tlist [Ttypes (Tint 6), Ttypes (Tint 7)], Ttypes (Tint 8)]
    print $ x
    let y = ["[", "["]
    let a = cnt "[" y
    print $ a
    code <- getLine
    let x = words code
    print $ x
    --let elem = makeTstring x
    --print $ elem
    let elem = parseToObj x []
    print $ elem
    

    -- gameloop (stack)
        --parse oga
        -- DO gameloop newstack

    --let x = line
    --putStrLn(x)
    --let f = words x
    --let elem = makeString f
    --print $ (fst elem)
    --print x
    --let a = Ttypes (Tint 5)
    --print $ a
 -}   
