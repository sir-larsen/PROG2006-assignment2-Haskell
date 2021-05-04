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

-- | Creating the tokens HUSK Å KOMMENTER VEKK DENNE IGJEN
parse :: [String] -> Stack -> Stack
parse [] stack = stack
parse (x:xs) stack

    -- Check string
    | x == "\"" = do
        let elem = makeTstring xs
        let newStack = push stack (fst elem)
        parse (snd elem) newStack




makeTstring :: [String] -> (StackElem, [String])
makeTstring s = do
    let stop = elemIndex "\"" s
    let str  = [s !! i | i <- [0..length s], i < fromJust stop]
    let rstr = drop (length str + 1) s
    case isJust stop of
        True  -> (Ttypes (Tstring $ unwords str), rstr)
        False -> error "Thou hath encountered syntax error. No closing symbol for string jest"
        
makeTint :: String -> Maybe StackElem
makeTint s
    | isInt s == True = Just (Ttypes (Tint (read s :: Int)))
    | otherwise = Nothing

makeTfloat :: String -> Maybe StackElem
makeTfloat s
    | isFloat s == True = Just (Ttypes (Tfloat (read s :: Float)))
    | otherwise = Nothing

makeTbool :: String -> Maybe StackElem
makeTbool s
    | isBool s == True = Just (Ttypes (Tbool (read s :: Bool)))
    | otherwise = Nothing

isBool :: String -> Bool
isBool x
    | x == "True" = True
    | x == "False" = True
    | otherwise = False

isInt :: String -> Bool
isInt ""  = False
isInt "." = False
isInt xs =
    case dropWhile isDigit xs of
        "" -> True
        _  -> False

isFloat :: String -> Bool
isFloat ""  = False
isFloat "." = False
isFloat xs  =
    case dropWhile isDigit xs of
        "" -> True
        ('.':ys) -> all isDigit ys
        _ -> False

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
