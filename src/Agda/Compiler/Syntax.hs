module Agda.Compiler.Syntax where
import Language.Java.Syntax (BlockStmt)

-- I don't think I will need this file at all tbh
data JavaEntry = 
    JavaDecl {block :: BlockStmt}
    | JavaDecl2 {block :: BlockStmt, body :: [BlockStmt]}



data JavaModule = 
    JavaModule {
        entries :: [JavaEntry]
        }