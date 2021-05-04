module Main where

import Interpreter
import Data.Char(toUpper)

main :: IO ()
--main = someFunc
--main = interact $ unlines . map processLine . lines
--main = interact (map toUpper)
main = do 
    putStrLn "test"
    --let u = Ttypes (Tint 5)
    --addStack u
    --line <- getContents
    code <- getLine
    let x = words code
    --let elem = makeTstring x
    --print $ elem
    let elem = parse x []
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
    
