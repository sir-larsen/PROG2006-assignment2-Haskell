module Interpreter where

import Text.Read
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

data StackElem = Ttypes Ttypes | Tlist[StackElem]

--type Stack = [Maybe StackElem]
type Stack = [StackElem]
type ProgState = State Stack Stack

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


