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
data StackElem = Ttypes Ttypes | Tlist[StackElem] deriving Show

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

    -- | x == "[" = do
    --    let elem = makeTlist xs
    --    let newStack = push stack (head (head elem))
    --    let remainingString = cutLength xs
    --    parse xs newStack


    -- Check for int
    | isJust (makeTint x) = do
        let newStack = push stack (fromJust $ makeTint x)
        parse xs newStack

    -- Checks float
    | isJust (makeTfloat x) = do
        let newStack = push stack (fromJust $ makeTfloat x)
        parse xs newStack

    --Checks for bool
    | isJust (makeTbool x) = do
        let newStack = push stack (fromJust $ makeTbool x)
        parse xs newStack
    
    | otherwise = error ("Neeey, there hath been a parsing error: Cannot parse " ++ "\"" ++ x ++ "\"")

parseToObj :: [String] -> [StackElem] -> [StackElem]
parseToObj [] lst = lst
parseToObj (x:xs) lst
    
    | x == "\"" = do
        let elem = makeTstring xs
        --let conLst = Tlist [fst elem]
        let newLst = lst ++ [fst elem]
        parseToObj (snd elem) newLst
    
    -- | x == "[" = makeTlist

    | isJust (makeTint x) = do
        let newLst = lst ++ [(fromJust $ makeTint x)]
        parseToObj xs newLst
    
    
    | isJust (makeTfloat x) = do
        let newLst = lst ++ [(fromJust $ makeTfloat x)]
        parseToObj xs newLst
    
    
    | isJust (makeTbool x) = do
        let newLst = lst ++ [(fromJust $ makeTbool x)]
        parseToObj xs newLst

    | otherwise = error ("Something is fucked regarding the parsing of types within lists: Cannot parse " ++ "\"" ++ x ++ "\"")



--makeTlist :: [String] -> ([StackElem], [String])
--makeTlist :: Monad m => [String] -> m ([StackElem], [String])
makeTlist :: Monad m => [[Char]] -> m [StackElem]
makeTlist s = do
    let numNest = cnt "[" s
    let stop = last (findIndices (=="]") s)
    let lst = [s !! i | i <- [0..length s], i < stop]
    let remaining = drop (length lst + 1) s
    let completeLst = parseToObj lst []-- send lst til parseToObj
    return completeLst

--cutLength :: Monad m => [[Char]] -> m [[Char]]
cutLength s = do
    let numNest = cnt "[" s
    let stop = last (findIndices (=="]") s)
    let lst = [s !! i | i <- [0..length s], i < stop]
    let remaining = drop (length lst + 1) s
    return remaining


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

-- | Checking how many times a thing is in a list 3.
cnt ::  (Eq a) => a -> [a] -> Int
cnt a [] = 0
cnt a (x:xs) -- same as using head
    | a == x    = 1 + cnt a xs
    | otherwise = cnt a xs

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
