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
instance Show Ttypes where
    show (CustomNuma i) = show i
    show (Tbool i) = "Tbool " ++ show i
    show (Tstring i) = "Tstring " ++ show i
  
    
-- | Instance making our stack look as desired when printed
instance Show StackElem where
    show (Ttypes i) = show i
    show (Tlist i) = show i

-- | Prints out our stack type
printStack :: Stack -> IO()
printStack s = print s

toWords :: String -> [String]
toWords x = words x

-- | Creating the tokens
parse :: [String] -> Stack -> Stack
parse [] stack = stack
parse (x:xs) stack

    -- + operator for ints and floats
    | x == "+" = do
        let newStack = add stack
        parse xs newStack

    -- - operator for ints and floats
    | x == "-" = do
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
                let (a, b) = makeTint2 s
                let newStack2 = push newStack (a)
                parse xs newStack2
            _ -> error "Failed to parse int"
    
    -- Takes a float in a string and turns it into a Tfloat
    | x == "parseFloat" = do
        let (popped, newStack) = pop stack
        case popped of
            (Ttypes (Tstring s)) -> do
                let (a, b) = makeTfloat2 s
                let newStack2 = push newStack (a)
                parse xs newStack2
            _ -> error "Failed to parse float"

    -- Parses our strings to Bstrings
    | x == "\""= do
        let newStack = push stack (fst bstring)
        parse (snd bstring) newStack

    -- Parsing lists
    | x == "[" = do
        let list = makeTlist xs
        let newStack = push stack (Tlist(fst list))
        parse (snd list) newStack

    -- Parses our ints to Bpints
    | intbool == True = do
        let newStack = push stack (bint)
        parse xs newStack

    -- Parses our floats to Bpfloats
    | floatbool == True = do
        let newStack = push stack (bfloat)
        parse xs newStack

    -- Parses our bools to Bpbools
    | boolbool == True = do
        let newStack = push stack (bbool)
        parse xs newStack

    -- Error on illegal input
    | otherwise = error "Error. Thou may not enter illegal datatypes!"
    where
        bstring = makeTstring xs
        (bint, intbool) = makeTint2 x
        (bfloat, floatbool) = makeTfloat2 x
        (bbool, boolbool) = makeTbool2 x

-- Parsing of the objects within a list
parseToObj :: [String] -> [StackElem] -> [StackElem]
parseToObj [] lst = lst
parseToObj (x:xs) lst
    
    | x == "\"" = do
        let elem = bstring
        --let conLst = Tlist [fst elem]
        let newLst = lst ++ [fst elem]
        parseToObj (snd elem) newLst

    | x == "[" = do
        let elem = makeTlist xs
        --let newLst = [(Tlist (fst elem))] ++ lst
        let newLst = lst ++ [(Tlist (fst elem))]
        parseToObj (snd elem) newLst

    |  intbool == True = do
        let newLst = lst ++ [bint]
        parseToObj xs newLst

    |  floatbool == True = do
        let newLst = lst ++ [bfloat]
        parseToObj xs newLst

    |  boolbool == True = do
        let newLst = lst ++ [bbool]
        parseToObj xs newLst

    | otherwise = error ("Error cannot parse: " ++ "\"" ++ x ++ "\"")

    where
        bstring = makeTstring xs
        (bint, intbool) = makeTint2 x
        (bfloat, floatbool) = makeTfloat2 x
        (bbool, boolbool) = makeTbool2 x

-- | The multiplication function
mult :: Stack -> Stack
mult stack = newStack
    where
        (secondEl, firstPop) = pop stack
        (firstEl, secondPop) = pop firstPop
        result = mult2 firstEl secondEl
        newStack = push secondPop (result)

-- | utilized by the mult function to use the * operator
mult2 :: StackElem -> StackElem -> StackElem
mult2 a b = do
    case a of
        (Ttypes( CustomNuma i)) -> case b of
            (Ttypes( CustomNuma y)) -> (Ttypes ( CustomNuma (i * y)))
            _ -> undefined
        _ -> undefined

-- | The function used for addition
add :: Stack -> Stack
add stack = newStack
    where
        (secondEl, firstPop) = pop stack
        (firstEl, secondPop) = pop firstPop
        result = add2 firstEl secondEl
        newStack = push secondPop (result)

-- | utilized by the add function to use the + operator
add2 :: StackElem -> StackElem -> StackElem
add2 a b = do
      case a of
        (Ttypes( CustomNuma i)) -> case b of
            (Ttypes( CustomNuma y)) -> (Ttypes ( CustomNuma (i + y))) 
            _ -> undefined
        _ -> undefined

-- | Our subtraction function
sub :: Stack -> Stack
sub stack = newStack
    where
        (secondEl, firstPop) = pop stack
        (firstEl, secondPop) = pop firstPop
        result = sub2 firstEl secondEl
        newStack = push secondPop (result)

-- | utilized by the sub function to use the - operator
sub2 :: StackElem -> StackElem -> StackElem
sub2 a b = do
      case a of
        (Ttypes( CustomNuma i)) -> case b of
            (Ttypes( CustomNuma y)) -> (Ttypes ( CustomNuma (i - y))) 
            _ -> undefined
        _ -> undefined

-- | our division function
diva :: Stack -> Stack
diva stack = newStack
    where
        (secondEl, firstPop) = pop stack
        (firstEl, secondPop) = pop firstPop
        result = div2 firstEl secondEl
        newStack = push secondPop (result)

-- | utilized by diva to use the / operator
div2 :: StackElem -> StackElem -> StackElem
div2 a b = do
      case a of
        (Ttypes( CustomNuma i)) -> case b of
            (Ttypes( CustomNuma y)) -> (Ttypes ( CustomNuma (i / y))) 
            _ -> undefined
        _ -> undefined

-- | function for bool operators && and ||
boolOperation :: Stack -> String -> Stack
boolOperation stack string = do
    let (firstElem, stack2) = pop stack
    let (secondElem, stack3) = pop stack2
    case string of
        "&&" -> if firstElem == (Ttypes (Tbool(False))) || secondElem == (Ttypes (Tbool(False))) then
                push stack3 (Ttypes (Tbool(False)))
                else push stack3 (Ttypes( Tbool(True) ))
        "||"-> if firstElem == (Ttypes (Tbool(True))) || secondElem == (Ttypes (Tbool(True))) then
                push stack3 (Ttypes (Tbool(True)))
                else push stack3 (Ttypes( Tbool(False) ))
        _-> undefined


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

-- | Makes an Integer of our own type
makeTint2 :: String -> (StackElem, Bool)
makeTint2 s
    | isInt s == True = ((Ttypes (CustomNuma( Tint (read s :: Int)))), True)
    | otherwise = ((Ttypes (Tnothing)), False)

-- | Makes a float of our own type
makeTfloat2 :: String ->(StackElem, Bool)
makeTfloat2 s
    | isFloat s == True = ((Ttypes (CustomNuma( Tfloat (read s :: Float)))), True)
    | otherwise = ((Ttypes (Tnothing)), False)
    
-- | Function for checking if an Int really is an Int. Inspired for stackoverflow
--https://stackoverflow.com/questions/30029029/haskell-check-if-string-is-valid-number
isInt :: String -> Bool
isInt ""  = False
isInt "." = False
isInt xs  =
  case dropWhile isDigit xs of
    ""       -> True
    _        -> False
-- | Same as above but for floats
isFloat :: String -> Bool
isFloat ""  = False
isFloat "." = False
isFloat xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

-- | Checks if a string represents a bool
isBool :: String -> Bool
isBool x
    | x == "True" = True
    | x == "False" = True
    | otherwise = False

-- | Creates a bool of our own datatype
makeTbool2 :: String -> (StackElem, Bool)
makeTbool2 s
    | isBool s == True = ((Ttypes (Tbool (read s :: Bool))), True)
    | otherwise = ((Ttypes (Tnothing)), False)





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