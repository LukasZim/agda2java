module Agda.Compiler.ToJava where
import Agda.Compiler.Syntax
import Agda.Compiler.Backend
import Agda.Syntax.Common
import Control.Monad.State
import Language.Java.Syntax (CompilationUnit (CompilationUnit), Decl (MemberDecl), TypeDecl (ClassTypeDecl), ClassDecl (ClassDecl), Ident (Ident), ClassBody (ClassBody), MemberDecl (MethodDecl), Modifier (Public, Static), FormalParam (FormalParam), Type (RefType), RefType (ArrayType, ClassRefType), ClassType (ClassType), VarDeclId (VarId), MethodBody (MethodBody), Block, BlockStmt (BlockStmt), Stmt (ExpStmt), Exp (Lit), Literal (Int))
import Data.Text

type JavaAtom = Text

buildBasicJava :: [Decl] -> CompilationUnit
buildBasicJava xs = CompilationUnit Nothing [] [ClassTypeDecl (ClassDecl [] (Ident "Main") [] Nothing [] (ClassBody xs))]

buildMainMethod :: Maybe Block -> Decl
buildMainMethod block = MemberDecl (MethodDecl [Public, Static] [] Nothing (Ident "main") [FormalParam [] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String", [])]))))) False (VarId (Ident "args"))] [] Nothing (MethodBody block))

newJavaDef :: QName -> Int -> Text
newJavaDef n i = pack $ show n

defToTreeless :: [Text] -> Definition -> ([Text], Maybe TTerm)
defToTreeless t def
    | defNoCompilation def ||
      not (usableModality $ getModality def) = (t, Nothing)
    | otherwise = do
        let f = defName def
        case theDef def of
            Axiom {} -> do
                let f' = newJavaDef f 0
                (f' : t, Nothing)
            Primitive {} -> do
                let f' = newJavaDef f 0
                (f' : t, Nothing)
            DataOrRecSig n -> (t, Nothing)
            GeneralizableVar -> (t, Nothing)
            AbstractDefn de -> (t, Nothing)
            Function cls m_cc m_st m_com cls' fi m_qns ia de m_pro set m_b
                    m_eli m_qn
                -> (t, Nothing)
            Datatype n i m_cl qns so m_qns ia qns' -> (t, Nothing)
            Record n m_cl ch b dos te m_qns ee poc m_in ia ck -> (t, Nothing)
            Constructor n i ch qn ia in' ck m_qns ifs m_bs -> (t, Nothing)
            PrimitiveSort s so -> (t, Nothing)

-- type ToJavaM x = StateT ToJavaState (ReaderT TojavaEnv TCM) x

-- data ToJavaState = ToJavaState
--     {
--         toJavaFresh :: [Text],
--         toJavaDefs :: 
--     }

-- defToTreeless :: Definition -> ToJavaM (Maybe ())



toJava :: TTerm -> [Text] -> [BlockStmt]
toJava term texts = 
    case term of  
        TVar n -> do
            let name = texts !! n
            [BlockStmt $ ExpStmt $ Lit $ Int $ toInteger n]
        TDef qn -> []
        TApp tt tts -> []
        TLam tt -> []
        TLit lit -> []
        TCon qn -> []
        TLet tt tt' -> []
        TCase n ci tt tas -> []
        TUnit -> []
        TSort -> []
        TErased -> []
        TCoerce tt -> []
        TError te -> []
        _ -> []
