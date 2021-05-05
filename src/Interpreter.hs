module Interpreter where

import Text.Read (readMaybe)
import Control.Monad.State.Lazy
import Data.Char
import Data.Maybe
import Data.List

-- | Custom datatypes for the interpreter
data Ttypes = CustomNuma CustomNum
              | Tstring String
              | Tbool   Bool
              | Tnothing
              deriving (Eq)

-- | The numerical datatypes
data CustomNum = Tint Int
                 | Tfloat Float
                 deriving (Show, Eq)

-- | Stack elements consisting of the datatypes
data StackElem = Ttypes Ttypes | Tlist[StackElem] deriving (Eq)

-- The stack itself
type Stack = [StackElem]

-- | Numerical instance letting us use the +,* and - operators across our types
instance Num CustomNum where
    (Tint i) + (Tint i2) = Tint (i + i2)
    (Tfloat i) + (Tfloat i2) = Tfloat (i + i2)
    (Tint i) + (Tfloat i2) = Tfloat (fromIntegral(i) + i2)
    (Tfloat i) + (Tint i2) = Tfloat (i + fromIntegral(i2))

    (Tint i) - (Tint i2) = Tint (i2 - i)
    (Tfloat i) - (Tfloat i2) = Tfloat (i2 - i)
    (Tint i) - (Tfloat i2) = Tfloat (fromIntegral(i) - i2)
    (Tfloat i) - (Tint i2) = Tfloat (i - fromIntegral(i2))

    (Tint i) * (Tint i2) = Tint (i * i2)
    (Tfloat i) * (Tfloat i2) = Tfloat (i * i2)
    (Tint i) * (Tfloat i2) = Tfloat (fromIntegral(i) * i2)
    (Tfloat i) * (Tint i2) = Tfloat (i * fromIntegral(i2))

-- | Fractional instance letting us use the / operator across our types
instance Fractional CustomNum where
    (Tint i) / (Tint i2) = Tfloat (fromIntegral(i2) / fromIntegral(i))
    (Tfloat i) / (Tfloat i2) = Tfloat (i2 / i)
    (Tint i) / (Tfloat i2) = Tfloat (fromIntegral(i) / i2)
    (Tfloat i) / (Tint i2) = Tfloat (i / fromIntegral(i2))

-- | Instance making our stack look as desired when printed
instance Show Btypes where
    show (CustomNuma i) = show i
    show (Tbool i) = "Tbool " ++ show i
    show (Tstring i) = "Tstring " ++ show i
  
    
-- | Instance making our stack look as desired when printed
instance Show BprogElement where
    show (Ttypes i) = show i

toWords :: String -> [String]
toWords x = words x

-- | Creating the tokens HUSK Å KOMMENTER VEKK DENNE IGJEN
parse :: [String] -> Stack -> Stack
parse [] stack = stack
parse (x:xs) stack

    -- + operator for ints and floats
    | x == "+" = do
        let newStack = add stack
        parse xs newStack

    -- - operator for ints and floats
    x == "-" = do
        let newStack = sub stack
        parse xs newStack
    
    -- Multiplication
    | x == "*" = do
        let newStack = mult stack
        parse xs newStack
    
    -- / operator for ints and floats
    | x == "/" = do
        let newStack = diva stack
        parse xs newStack

    -- Pop command for the stack
    | x == "pop" = do
        let (popped, newStack) = pop stack
        parse xs newStack

    -- Swap command for stack
    | x == "swap" = do
        let newStack = swap stack
        parse xs newStack

    -- Dup command for the stack
    | x == "dup" = do
        let newStack = dup stack
        parse xs newStack

    -- AND operator
    | x == "&&" = do
        let newStack = boolOperation stack x
        parse xs newStack
    
    -- OR operator
    | x == "||" = do
        let newStack = boolOperation stack x
        parse xs newStack
    
    -- Takes an integer in a string and turns it into a Tint
    | x == "parseInteger" = do
        let (popped, newStack) = pop stack
        case popped of
            (Ttypes (Tstring s)) -> do
                let a = fromJust (makeTint s)
                let newStack2 = push newStack (a)
                parse xs newStack2
            _ -> error "Failed to parse int"
    
    -- Takes a float in a string and turns it into a Tfloat
    | x == "parseFloat" = do
        let (popped, newStack) = pop stack
        case popped of
            (Ttypes (Tstring s)) -> do
                let a = fromJust (makeTfloat s)
                let newStack2 = push newStack (a)
                parse xs newStack2
            _ -> error "Failed to parse float"

    -- Check string
    | x == "\"" = do
        let elem = makeTstring xs
        let newStack = push stack (fst elem)
        parse (snd elem) newStack

    -- Parsing lists
    | x == "[" = do
        let elem = makeTlist xs
        let newStack = push stack (Tlist (fst elem))
        parse (snd elem) newStack


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
    
    | x == "[" = do
        let elem = makeTlist xs
        let newLst = [(Tlist (fst elem))] ++ lst
        parse (snd elem) newLst

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


makeTlist :: [String] -> ([StackElem], [String])
makeTlist s = do
    let numNest = cnt "[" s
    let stop2 = elemIndex "]" s
    let stop = last (findIndices (=="]") s)
    let lst = [s !! i | i <- [0..length s], i < stop]
    let remaining = drop (length lst + 1) s
    let completeLst = parseToObj lst []-- send lst til parseToObj
    --return completeLst
    case isJust stop2 of
        True  -> (completeLst, remaining)
        False -> error "Thou hath not closed thy list"

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

-- | Function for checking if an Int really is an Int. Inspired for stackoverflow
--https://stackoverflow.com/questions/30029029/haskell-check-if-string-is-valid-number
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

-- | Checking how many times a thing is in a list 3.
cnt ::  (Eq a) => a -> [a] -> Int
cnt a [] = 0
cnt a (x:xs) -- same as using head
    | a == x    = 1 + cnt a xs
    | otherwise = cnt a xs

push :: Stack -> StackElem -> Stack
push stack x = x : stack

push2 :: Stack -> [StackElem] -> Stack
push2 stack xs = stack ++ xs

-- | Pops an element off the stack and returns it
pop :: Stack -> (StackElem, Stack)
pop [] = error "CXannot pop empty stack dummy"
pop (x:xs) = (x, xs)

swap :: Stack -> Stack
swap (a:b:xs) = b : a : xs
swap _ = error "Cannot swap, less than two elements"

dup :: Stack -> Stack
dup (x:xs) = x : x : xs
dup _ = error "Cannot dup, stack empty"