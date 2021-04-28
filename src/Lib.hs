module Lib where

import Text.Read
import Control.Monad.State.Lazy


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Stack = [Either String Float]