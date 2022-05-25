{-# LANGUAGE FunctionalDependencies #-}
module Agda.Compiler.ToJava where
import Agda.Compiler.Syntax
import Agda.Compiler.Backend
import Agda.Syntax.Common
import Control.Monad.State
import Language.Java.Syntax
import Agda.Compiler.Treeless.Erase
import Data.Text
import Data.Map
import Data.Set

import Agda.Utils.Pretty (prettyShow)

import Language.Java.Pretty

import Agda.Syntax.Internal (ConHead(conName), Type)
import Control.Monad.Reader
import qualified Agda.Utils.Pretty as P
import qualified Data.Map as Map
import Agda.Compiler.Options (JavaOptions, javaEvaluation)
import Agda.Utils.Monad
import qualified Data.Set as Set
import Prelude hiding (null, empty)
import Agda.Utils.Maybe
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Control.Arrow ( first , second )
import Agda.Utils.Lens
import GHC.Tc.Utils.Instantiate (freshenCoVarBndrsX)
import Agda.Utils.Pretty
import Agda.Compiler.MAlonzo.Pretty
import qualified Data.Text as T

type JavaAtom = Text
type JavaForm = Decl
type JavaBlock = Block
type JavaClass = ClassDecl

buildBasicJava :: [Decl] -> CompilationUnit
buildBasicJava xs = CompilationUnit Nothing [] [ClassTypeDecl (ClassDecl [] (Ident "Main") [] Nothing [] (ClassBody xs))]

buildMainMethod :: Maybe Block -> Decl
buildMainMethod block = MemberDecl (MethodDecl [Public, Static] [] Nothing (Ident "main") [FormalParam [] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String", [])]))))) False (VarId (Ident "args"))] [] Nothing (MethodBody block))

-- buildMethod :: String -> [(RefType, String)] -> Maybe Block
buildMethodIO :: [String] -> Maybe (String, [String]) -> String -> [(String, [String], String)] -> Maybe Block -> [Modifier] -> IO Decl
buildMethodIO typeParam maybetype name params body modifiers = do
    liftIO do
        -- putStrLn "buildMethod"
        putStrLn $ Language.Java.Pretty.prettyPrint (x)
    return x
        where
            x = buildMethod typeParam maybetype name params body modifiers

buildMethod :: [String] -> Maybe (String, [String]) -> String -> [(String, [String], String)] -> Maybe Block -> [Modifier] ->  Decl
buildMethod typeParam maybetype name params body modifiers = x
    where
        x = MemberDecl $ MethodDecl modifiers (buildTypeParams typeParam) (getReturnType maybetype) (Ident name) (buildParams params) [] Nothing (MethodBody body)

        getReturnType :: Maybe (String, [String]) -> Maybe Language.Java.Syntax.Type
        getReturnType (Just (x, y)) = Just $ buildType x y
        getReturnType Nothing = Nothing

buildTypeParams :: [String] -> [TypeParam]
buildTypeParams = Prelude.map buildTypeParam

buildTypeParam :: String -> TypeParam
buildTypeParam str = TypeParam (Ident str) []

buildParams :: [(String , [String] , String)] -> [FormalParam]
buildParams = Prelude.map buildParam

buildParam :: (String , [String] , String) -> FormalParam
buildParam (javaType, args, name) = FormalParam [] (buildType javaType args) False (VarId $ Ident name)


-- doesn't handle types like int / float / whatever yet, only object types for now
buildType :: String -> [String] -> Language.Java.Syntax.Type
buildType name typearguments = RefType $ buildRefType name typearguments

buildRefType :: String -> [String] -> RefType
buildRefType name typearguments = ClassRefType $ ClassType [(Ident name, makeTypeArguments typearguments)]

makeTypeArguments :: [String] -> [TypeArgument]
makeTypeArguments = Prelude.map makeTypeArgument

makeTypeArgument :: String -> TypeArgument
makeTypeArgument name = ActualType $ ClassRefType $ ClassType [(Ident name , [])]


buildMainMethodMonad :: [Decl] -> ToJavaM CompilationUnit
buildMainMethodMonad b = do
    -- let intermediate x = buildBasicJava  [buildMainMethod(Just x)]
    return $ buildBasicJava  $ buildMainMethod (Just $ Block []) : b

-- doesn't work properly yet
makeJavaName :: QName -> ToJavaM JavaAtom
makeJavaName n = return $ pack $ getName n
    where
        getName :: QName -> String
        getName  = getName' . qnameName

        getName' :: Agda.Compiler.Backend.Name -> String
        getName' =  prettyShow

initToJavaState :: ToJavaState
initToJavaState = ToJavaState
    {
        toJavaFresh = freshVars
        , toJavaDefs = Map.empty
        , toJavaUsedNames = reservedNames
    }

freshVars :: [JavaAtom]
freshVars = Prelude.concat [ Prelude.map (<> i) xs | i <- pack "":Prelude.map (pack . show) [1..]]
    where
        xs = Prelude.map Data.Text.singleton ['a' .. 'z']

reservedNames :: Set JavaAtom
reservedNames = Set.fromList $ Prelude.map pack
    ["abstract","continue","for","new","switch",
        "assert","default","goto","package","synchronized",
        "boolean","do","if","private","this",
        "break","double","implements","protected","throw",
        "byte","else","import","public","throws",
        "case","enum","instanceof","return","transient",
        "catch","extends","int","short","try",
        "char","final","interface","static","void",
        "class","finally","long","strictfp","volatile",
        "const","float","native","super","while"]

withFreshVars :: Int -> ([JavaAtom] -> ToJavaM a) -> ToJavaM a
withFreshVars i f = do
  strat <- getEvaluationStrategy
  withFreshVars' strat i f

withFreshVars' :: EvaluationStrategy -> Int -> ([JavaAtom] -> ToJavaM a) -> ToJavaM a
withFreshVars' strat i f
  | i <= 0    = f []
  | otherwise = withFreshVar' strat $ \x -> withFreshVars' strat (i-1) (f . (x:))






initToJavaEnv :: JavaOptions -> ToJavaEnv
initToJavaEnv opts = ToJavaEnv opts []

runToJavaM :: JavaOptions -> ToJavaM a -> TCM a
runToJavaM opts =
    (`runReaderT` initToJavaEnv opts)
    . (`evalStateT` initToJavaState)


newJavaDef :: QName -> Int -> Text
newJavaDef n i = pack $ show n

setJavaDef2 :: QName -> Int -> [Bool] -> JavaAtom -> ToJavaM ()
setJavaDef2 n i bs a = modify $ \s -> s {toJavaDefs = Map.insert n (i, bs, a) (toJavaDefs s)}

newJavaDef2 :: QName -> Int -> [Bool] -> ToJavaM JavaAtom
newJavaDef2 n i bs = do
    -- unlessM (m Bool) (m ())
    unlessM (isNothing <$> lookupJavaDef n) __IMPOSSIBLE__
    a <- makeJavaName n
    setJavaDef2 n i bs a
    setNameUsed a
    return a



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

defToTreeless2 :: Definition -> ToJavaM (Maybe (Int, [Bool], JavaAtom, TTerm, [QName]))
defToTreeless2 def
    | defNoCompilation def ||
    not (usableModality $ getModality def) = return Nothing
    | otherwise = do
        let f = defName def
        -- reportSDoc "toJava2" 5 $ "Compiling definition:" <> prettyTCM f
        case theDef def of
            Axiom{} -> do
                -- liftIO do
                --     putStrLn "AXIOM"
                --     putStrLn $ prettyShow f
                f' <- newJavaDef2 f 0 []
                return Nothing
            DataOrRecSig n -> __IMPOSSIBLE__
            GeneralizableVar -> do
                liftIO do
                    putStrLn "generealzidable var"
                    putStrLn $ prettyShow f
                return Nothing
            AbstractDefn de -> __IMPOSSIBLE__
            d@Function {} | d ^. funInline -> do
                liftIO do
                    putStrLn "In Line Function"
                    putStrLn $ show f
                return Nothing
            Function a b c d e ff g h i j k l m n
                -> do
                    liftIO do
                        putStrLn "FUNCTION: "
                        putStrLn $ show n
                    strat <- getEvaluationStrategy
                    maybeCompiled <- liftTCM $ toTreeless strat f
                    -- liftIO do
                    --     putStrLn "FUNCTION: "
                    --     putStrLn $ prettyShow f
                    --     print maybeCompiled
                    case maybeCompiled of
                        Nothing -> return Nothing
                        Just body -> do
                            let (n, body') = lambdaView body
                            f' <- newJavaDef2 f n (Prelude.take n [])
                            return $ Just (n,[], f', body',[])
            Datatype q w e r t y u i -> do
                liftIO do
                    putStrLn "DATATYPE: "
                    putStrLn $ Prelude.concatMap prettyShow r
                    putStrLn $ prettyShow $ qnameName f
                return $ Just (0, [], pack $ prettyShow $ qnameName f, TDef f, r)
            Record n m_cl ch b dos te m_qns ee poc m_in ia ck -> return Nothing
            Constructor {conSrcCon = chead, conArity = nargs} -> do
                liftIO
                    do
                        putStrLn "CONSTRUCTOR: "
                        putStrLn $ prettyShow f
                        putStrLn $ prettyShow chead
                        print nargs
                let name = conName chead
                return $ Just (0, [], pack $ prettyShow f, TDef f, [])
            Primitive ia s cls fi m_cc -> do
                -- liftIO do
                --     putStrLn "Primitive"
                --     putStrLn $ prettyShow f
                f' <- newJavaDef2 f 0 []
                return Nothing
            PrimitiveSort s so -> do
                -- liftIO do
                --     putStrLn "primitiveSort"
                --     putStrLn $ prettyShow f
                return Nothing

-- type ToJavaM x = StateT ToJavaState (ReaderT TojavaEnv TCM) x

-- data ToJavaState = ToJavaState
--     {
--         toJavaFresh :: [Text],
--         toJavaDefs :: 
--     }

-- defToTreeless :: Definition -> ToJavaM (Maybe ())
lambdaView :: TTerm -> (Int, TTerm)
lambdaView v = case v of
  TLam    w -> first (1+) $ lambdaView w
  TCoerce w -> lambdaView w
  _         -> (0, v)

-- erasureInfo :: QName -> ToJavaM (Maybe [Bool])
-- erasureInfo f = liftTCM $ runE $ do
--   (bs, b) <- getFunInfo f
--   if erasable b
--     then return Nothing
--     else return (Just $ Prelude.map erasable bs)


toJava :: TTerm -> [Text] -> [BlockStmt]
toJava term texts =
    case term of
        TVar n -> do
            let name = texts !! n
            [BlockStmt $ ExpStmt $ Lit $ Int $ toInteger n]
            -- [BlockStmt $ ExpStmt $ Lit $ String $ unpack name]
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

javaDefine :: JavaAtom -> [JavaAtom] -> JavaBlock -> JavaForm
javaDefine f xs body = MemberDecl $ MethodDecl [Public] [] Nothing (Ident $ unpack f) (Prelude.map createFormalType xs) [] Nothing (MethodBody $ Just body)
    where
        createFormalType :: JavaAtom -> FormalParam
        createFormalType x = FormalParam [] (RefType (ClassRefType $ ClassType [(Ident "Object", [])])) False (VarId $ Ident $ unpack x)

javaDataType :: JavaAtom -> Maybe JavaAtom -> [JavaAtom] -> JavaForm
javaDataType name Nothing moreNames = MemberDecl $ MemberClassDecl $
    ClassDecl
        [Abstract, Static]
        (Ident $ unpack name)
        []
        Nothing
        []
        $ ClassBody [
            buildMethod
                ["T"]
                (Just ("T", []))
                "match"
                [("Visitor", ["T"], "visitor")]
                Nothing
                [Abstract],
            createVisitorInterface moreNames,
            javaCreateConstructor name []
            ]
javaDataType name (Just x) moreNames = MemberDecl $ MemberClassDecl $ ClassDecl [] (Ident $ unpack name) [] (Just $ ClassRefType $ ClassType [(Ident $ unpack x, [])]) [] $ ClassBody [javaCreateConstructor name []]
-- doesn't work with arguments yet
javaCreateConstructor :: JavaAtom -> [JavaForm] -> JavaForm
javaCreateConstructor name args = MemberDecl $ ConstructorDecl [Public] [] (Ident $ unpack name) [] [] $ ConstructorBody Nothing []

createVisitorInterface :: [JavaAtom] -> JavaForm
createVisitorInterface constructors = MemberDecl $ MemberInterfaceDecl $
    InterfaceDecl
        InterfaceNormal
        []
        (Ident "Visitor")
        (buildTypeParams ["T"])
        []
        (InterfaceBody $ Prelude.map makeMethodFrom constructors)
            where
                makeMethodFrom :: JavaAtom -> MemberDecl
                makeMethodFrom atom = MethodDecl [] [] (Just $ buildType "T" []) (Ident $unpack atom) [] [] Nothing (MethodBody Nothing)


javaUseConstructor :: JavaAtom -> JavaAtom -> [JavaAtom] -> JavaForm
javaUseConstructor constructorType name args = InitDecl False $ Block [LocalVars [] (RefType javaType) [VarDecl (VarId $ Ident $ unpack name) (Just $ InitExp $ InstanceCreation [] (TypeDeclSpecifier javaClassType) [] Nothing)]]
    where
        javaType = ClassRefType javaClassType
        javaClassType = ClassType[(Ident $ unpack constructorType, [])]

-- \x -> FormalParam [] (RefType $ ClassType [(Ident "Object", [])]) False x)

data ToJavaState = ToJavaState
    {
    toJavaFresh :: [JavaAtom]
    , toJavaDefs :: Map QName (Int, [Bool], JavaAtom)
    , toJavaUsedNames :: Set JavaAtom
    }
data ToJavaEnv = ToJavaEnv
    {
        toJavaOptions :: JavaOptions
        , toJavaVars :: [JavaBlock]
    }

type ToJavaM a = StateT ToJavaState (ReaderT ToJavaEnv TCM) a

dropArgs :: [Bool] -> [a] -> [a]
dropArgs bs xs = Prelude.map snd $ Prelude.filter (not . fst) $ Prelude.zip bs xs

lookupJavaDef :: QName -> ToJavaM (Maybe (Int, [Bool], JavaAtom))
lookupJavaDef n = Map.lookup n <$> gets toJavaDefs

-- doesn't do anything yet!!!@
forceIfLazy :: EvaluationStrategy -> a -> a
forceIfLazy strat = id

freshJavaAtom :: ToJavaM JavaAtom
freshJavaAtom = do
    names <- gets toJavaFresh
    case names of
      [] -> fail "No More Vars!?!?"
      x : names' -> do
          modify $ \st -> st {toJavaFresh = names'}
          ifM (isNameUsed x) freshJavaAtom $ {-otherwise-} do
              setNameUsed x
              return x

addBinding :: JavaBlock -> ToJavaEnv -> ToJavaEnv
addBinding x env = env { toJavaVars = x : toJavaVars env}

withFreshVar :: (JavaAtom -> ToJavaM a) -> ToJavaM a
withFreshVar f = do
    strat <- getEvaluationStrategy
    withFreshVar' strat f

withFreshVar' :: EvaluationStrategy -> (JavaAtom -> ToJavaM a) -> ToJavaM a
withFreshVar' strat f = do
    x <- freshJavaAtom
    local (addBinding $ forceIfLazy strat (Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident (unpack x)]])) $ f x

getEvaluationStrategy :: ToJavaM EvaluationStrategy
getEvaluationStrategy = reader $ javaEvaluation . toJavaOptions


isNameUsed :: JavaAtom -> ToJavaM Bool
isNameUsed x = Set.member x <$> gets toJavaUsedNames

getVar :: Int -> ToJavaM JavaBlock
getVar i = reader $ (!! i) . toJavaVars

-- javaApps :: JavaBlock -> [JavaBlock] -> JavaBlock
-- javaApps f args = foldl (\x y -> )

setNameUsed :: JavaAtom -> ToJavaM ()
setNameUsed x = modify \s ->
    s {toJavaUsedNames = Set.insert x (toJavaUsedNames s)}

class ToJava2 a b | a -> b where
    toJava2 :: a -> ToJavaM b

instance ToJava2 QName (Int, [Bool], JavaAtom) where
    toJava2 n = do
        r <- lookupJavaDef n
        case r of
            Nothing -> fail $ "unbound name" <> show (P.pretty n)
            Just a -> return a

instance ToJava2 TAlt JavaBlock where
    toJava2 alt = case alt of
      TACon c nargs v -> withFreshVars' EagerEvaluation nargs $ \ xs -> do
          (i, bs, c') <- toJava2 c
          body <- toJava2 v
          return $ Block []
      TAGuard {} -> __IMPOSSIBLE__
      TALit {} -> __IMPOSSIBLE__

instance ToJava2 (Int, [Bool], JavaAtom, TTerm, [QName]) JavaForm where
    toJava2 (n, bs, f, body, names) =
        case body of
            TDef {} ->
                withFreshVars n $ \ xs ->
                    return $ javaDataType name parent constructors
                        where
                            split :: JavaAtom -> [JavaAtom]
                            split  = Data.Text.splitOn (pack ".")

                            lookup :: Int -> [JavaAtom] -> Maybe JavaAtom
                            lookup _ [] = Nothing
                            lookup 0 (x : xs) = Just x
                            lookup i (_ : xs) = lookup (i-1) xs

                            lst = split f
                            len = Prelude.length lst
                            name = lst !! (len - 1)
                            parent = lookup (len - 2) lst

                            constructors = Prelude.map (pack . prettyShow . qnameName) names
            TCon c ->
                withFreshVars n $ \ xs ->
                    return $ javaUseConstructor (pack $ prettyShow $ qnameName c) f xs
            -- TCase num caseType term alts ->
            --     withFreshVars n $ \ xs ->
            --     -- from caseType get the type of the return statement?
            --     -- create the branches of the case statements by if expressions?
            --         javaCaseCreate f xs <$> toJava2 body
            --             where
            --                 parsedType = getTypeFromCaseInfo caseType
            --                 parsedAlts = Prelude.map toJava2 alts
            --                 cases = Prelude.map toJava2 parsedAlts

            _ ->
                withFreshVars n $ \ xs ->
                    javaDefine f (dropArgs bs xs) <$> toJava2 body

getTypeFromCaseInfo :: CaseInfo -> JavaAtom
getTypeFromCaseInfo (CaseInfo b ct) = case ct of
                                            CTData quan qn -> pack $ prettyShow $ qnameName qn
                                            CTNat -> pack "int"
                                            CTInt -> pack "int"
                                            CTChar -> pack "char"
                                            CTString -> pack "String"
                                            CTFloat -> pack "float"
                                            CTQName -> pack "CTQNameIdkHowToTranslateThis"

        -- withFreshVars n $ \ xs ->
        --     javaDefine f (dropArgs bs xs) <$> toJava2 body

-- instance ToJava2 (Int, [Bool], JavaAtom, TTerm) JavaClass where


instance ToJava2 TTerm JavaBlock where
    toJava2 v = case v of
        TVar n -> do
            x <- getVar n
            -- return $ Block []
            return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        TPrim tp -> return $ Block []
        TDef qn -> return $ Block []
        TApp tt tts -> return $ Block []
        TLam tt -> return $ Block []
        TLit lit -> return $ Block []
        -- TCon qn -> return $ Block []
        TLet tt tt' -> return $ Block []
        TCase n ci tt tas -> return $ Block []
        -- TPrim tp -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show tp]]
        -- TDef qn -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show qn]]
        -- TApp n tts -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TLam n -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TLit n -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        TCon n -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ prettyShow $ qnameName n]]
        -- TLet n tt' -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TCase n ci tt tas -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]

        TUnit -> return $ Block []
        TSort -> return $ Block []
        TErased -> return $ Block []
        TCoerce tt -> return $ Block []
        TError te -> return $ Block []
