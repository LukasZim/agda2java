module Main where

import Lib
import Agda.Compiler.ToJava
import Agda.Compiler.Compiler
import Agda.Compiler.Backend
import Agda.Main

main :: IO ()
-- main = someFunc
main = runAgda [javaBackend]
