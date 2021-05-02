module Main where

import Interpreter
import Data.Char(toUpper)

main :: IO ()
--main = someFunc
--main = interact $ unlines . map processLine . lines
--main = interact (map toUpper)
main = do
    line <- getContents
    let x = line
    --putStrLn(x)
    print x