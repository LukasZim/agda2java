module Lib
    ( someFunc
    ) where

import Agda.Compiler.Compiler 

someFunc :: IO ()
someFunc = putStrLn ("someFunc" ++ test)
