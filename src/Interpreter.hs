module Interpreter where

import Text.Read (readMaybe)
import Control.Monad.State.Lazy
import Data.Char
import Data.Maybe
import Data.List


data Ttypes = Tint      Int
              | Tfloat  Float
              | Tstring String
              | Tbool   Bool
--              | Tlist   [Ttypes]
--              | TemptyList
              deriving Show

--data StackElem = Ttypes Ttypes | Tlist[StackElem]
data StackElem = Ttypes Ttypes | Tlist[Ttypes] deriving Show

--type Stack = [Maybe StackElem]
type Stack = [StackElem]
--type ProgState = State Stack Stack

someFunc :: IO ()
someFunc = putStrLn "someFunc"
--type Stack = [Either String Float]

--processLine :: String -> String
--processLine line = unwords $ map show $ evalState (processTokens $ words line) ([])

toWords :: String -> [String]
toWords x = words x

--processTokens :: [String] -> ProgState
--processTokens [] = do
--    stack <- get
--    return stack

--processTokens (x:xs)
--    | x == 

tokenCheck :: [String] -> IO ()
tokenCheck (x:xs) = do
    putStrLn(x)
    putStrLn(tail x)
    --print (head x)
    --let x = words com
    --putStrLn x

--addStack :: StackElem -> ProgState
--addStack :: StackElem -> ProgState
--addStack x = do
--    stack <- get
--    let newStack = x : stack
--    put newStack
--    return newStack

push :: Stack -> StackElem -> Stack
push stack x = x : stack

pop :: Stack -> Stack
pop [] = error "CXannot pop empty stack dummy"
pop x = tail x

swap :: Stack -> Stack
swap (a:b:xs) = b : a : xs
swap _ = error "Cannot swap, less than two elements"

dup :: Stack -> Stack
dup (x:xs) = x : x : xs
dup _ = error "Cannot dup, stack empty"

--tCheck xs = head xs
