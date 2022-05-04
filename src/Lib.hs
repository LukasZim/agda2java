module Lib
    ( someFunc
    ) where

import Agda.Compiler.ToJava

someFunc :: IO ()
someFunc = putStrLn ("someFunc" ++ test)
