module Agda.Compiler.ToJava where

import Agda.Compiler.Backend
import Agda.Compiler.Options

test :: String
test = "lumfao"

javaBackend :: Backend
javaBackend = Backend javaBackend'

-- javaBackend' :: Backend' JavaOptions JavaOptions JavaEnv JavaModule [JavaEntry]
javaBackend' :: Backend' JavaOptions JavaOptions () () [JavaEntry]
javaBackend' = Backend' {
    backendName = "agda2java", 
    backendVersion = Nothing, 
    options = defaultJavaOptions, 
    commandLineFlags = javaCommandLineFlags, 
    isEnabled = const True, 
    preCompile = javaPreCompile, 
    postCompile = javaPostCompile, 
    preModule = \_ _ _ _ -> return $ Recompile JavaEnv {}, 
    postModule = javaPostModule, 
    compileDef = javaCompileDef, 
    scopeCheckingSuffices = False, 
    mayEraseType = const $ return True
    } 