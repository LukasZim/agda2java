{-# LANGUAGE FunctionalDependencies #-}
module Agda.Compiler.ToJavaBackup where
import Agda.Compiler.Syntax
import Agda.Compiler.Backend
    ( EvaluationStrategy(EagerEvaluation),
      toTreeless,
      funInline,
      Name,
      QName(qnameName),
      CaseInfo(CaseInfo),
      CaseType(CTQName, CTData, CTNat, CTInt, CTChar, CTString, CTFloat),
      TAlt(TALit, TACon, TAGuard),
      TTerm(..),
      Definition(defNoCompilation, defName, theDef),
      Defn(PrimitiveSort, Axiom, DataOrRecSig, GeneralizableVar,
           AbstractDefn, Function, Datatype, Record, Constructor, Primitive,
           conSrcCon, conArity),
      MonadTCM(liftTCM),
      TCM,
      HasConstInfo(getConstInfo) )
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
        , toJavaCons = Map.empty
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



-- setJavaDef :: QName -> Int -> [Bool] -> JavaAtom -> ToJavaM ()
-- setJavaDef n i bs a = modify $ \s -> s {toJavaDefs = Map.insert n (i, bs, a) (toJavaDefs s)}

setJavaDef :: QName -> ToJavaDef -> ToJavaM ()
setJavaDef n def = do
    modify $ \s -> s {toJavaDefs = Map.insert n def (toJavaDefs s)}

newJavaDef :: QName -> Int -> [Bool] -> ToJavaM JavaAtom
newJavaDef n i bs = do
    a <- makeJavaName n
    setJavaDef n (ToJavaDef a i bs)
    setNameUsed a
    return a

-- newJavaDef :: QName -> Int -> [Bool] -> ToJavaM JavaAtom
-- newJavaDef n i bs = do
--     -- unlessM (m Bool) (m ())
--     unlessM (isNothing <$> lookupJavaDef n) __IMPOSSIBLE__
--     a <- makeJavaName n
--     setJavaDef2 n i bs a
--     setNameUsed a
--     return a




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
                f' <- newJavaDef f 0 []
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
                        putStrLn $ Agda.Compiler.MAlonzo.Pretty.prettyPrint $ qnameName f
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
                            f' <- newJavaDef f n (Prelude.take n [])
                            return $ Just (n,[], f', body',[])
            Datatype q w e r t y u i -> do
                let eraseTag = Prelude.length r == 1
                liftIO do
                    putStrLn "DATATYPE: "
                    putStrLn $ Prelude.concatMap prettyShow r
                    -- print q
                    -- print w
                    -- print e
                    -- print t
                    -- putStrLn $ Prelude.concatMap prettyShow y
                    -- print u
                    -- print i
                    -- putStrLn $ prettyShow $ qnameName f
                forM_ r $ \c -> do
                    cdef <- theDef <$> getConstInfo c
                    case cdef of
                        Constructor { conSrcCon = chead, conArity = nargs } -> processCon chead nargs eraseTag
                        _ -> __IMPOSSIBLE__

                return Nothing
                -- return $ Just (0, [], pack $ prettyShow $ qnameName f, TDef f, r)
            Record n m_cl ch b dos te m_qns ee poc m_in ia ck -> return Nothing
            Constructor {conSrcCon = chead, conArity = nargs} -> do
                liftIO
                    do
                        putStrLn "CONSTRUCTOR: "
                        putStrLn $ prettyShow f
                        putStrLn $ prettyShow chead
                        print nargs
                let name = conName chead
                return Nothing
                -- return $ Just (0, [], pack $ prettyShow f, TDef f, [])
            Primitive ia s cls fi m_cc -> do
                f' <- newJavaDef f 0 []
                return Nothing
            PrimitiveSort s so -> do
                return Nothing
            where
                processCon :: ConHead -> Int -> Bool -> ToJavaM ()
                processCon chead nargs b = do
                    void $ newJavaCon (conName chead) nargs b

newJavaCon :: QName -> Int -> Bool -> ToJavaM JavaAtom
newJavaCon n i b = do
    a <- makeJavaName n
    setJavaCon n (ToJavaCon a i b)
    setNameUsed a
    return a


setJavaCon :: QName -> ToJavaCon -> ToJavaM ()
setJavaCon n con = do
    modify $ \s -> s {toJavaCons = Map.insert n con (toJavaCons s)}


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
-- Datatype
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
-- For the constructor types
javaDataType name (Just x) moreNames = MemberDecl $ MemberClassDecl $
    ClassDecl
        [Static]
        (Ident $ unpack name)
        []
        (Just $ ClassRefType $ ClassType [(Ident $ unpack x, [])])
        []
        (ClassBody [
            javaCreateConstructor name [],
            buildMethod
                ["T"]
                (Just ("T", []))
                "match"
                [("Visitor", ["T"], "visitor")]
                (Just $ Block [
                    BlockStmt $ Return (Just $ MethodInv $ PrimaryMethodCall (ExpName $ Language.Java.Syntax.Name [Ident "visitor"]) [] (Ident $ unpack name) [] )
                ])
                []
        ])
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

javaCaseCreate :: JavaAtom -> [JavaAtom] -> [JavaForm] -> Maybe JavaBlock -> JavaForm
javaCaseCreate str strings asdf block = buildMethod [] Nothing (unpack str) []  block [Public]

--                  name
-- javaCreateMatch :: JavaAtom -> [JavaForm] -> JavaForm
-- javaCreateMatch name instances = 


-- \x -> FormalParam [] (RefType $ ClassType [(Ident "Object", [])]) False x)

data ToJavaState = ToJavaState
    {
    toJavaFresh :: [JavaAtom]
    , toJavaDefs :: Map QName ToJavaDef
    , toJavaCons :: Map QName ToJavaCon
    , toJavaUsedNames :: Set JavaAtom
    }
data ToJavaEnv = ToJavaEnv
    {
        toJavaOptions :: JavaOptions
        , toJavaVars :: [JavaBlock]
    }
data ToJavaCon = ToJavaCon JavaAtom Int Bool 
data ToJavaDef = ToJavaDef JavaAtom Int [Bool]
type ToJavaM a = StateT ToJavaState (ReaderT ToJavaEnv TCM) a

dropArgs :: [Bool] -> [a] -> [a]
dropArgs bs xs = Prelude.map snd $ Prelude.filter (not . fst) $ Prelude.zip bs xs

lookupJavaDef :: QName -> ToJavaM ToJavaDef
lookupJavaDef n = do
    r <- gets (Map.lookup n . toJavaDefs)
    maybe __IMPOSSIBLE__ return r

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

-- instance ToJava2 QName (Int, [Bool], JavaAtom) where
--     toJava2 n = do
--         r <- lookupJavaDef n
--         case r of
--             Nothing -> fail $ "unbound name" <> show (P.pretty n)
--             Just a -> return a

instance ToJava2 (TAlt, JavaAtom) JavaForm where
    toJava2 (alt, typeInfo) = case alt of
      TACon c nargs v -> withFreshVars' EagerEvaluation nargs $ \ xs -> do
        --   (i, bs, c') <- toJava2 c
            body <- toJava2 v
        --   return $ MemberDecl $ FieldDecl [] (buildType (prettyShow $ qnameName c) []) [VarDecl (VarId $ Ident "test") Nothing]
            return $ buildMethod
                []
                (Just (unpack typeInfo, []))
                (prettyShow $ qnameName c)
                []
                (Just body)
                [Public]

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




                -- withFreshVars n $ \ xs ->
                -- -- from caseType get the type of the return statement?
                -- -- create the branches of the case statements by if expressions?
                --     let x = num
                --         y = caseType
                --         z = term
                --         zz = alts in
                --     do
                --         x <-  Prelude.map toJava2 (Prelude.zip alts (createListOfLen alts parsedType))
                --         return x
                --     javaCaseCreate f xs parsedAlts2 qwer
                --         where
                --             qwer =toJava2 body
                --             parsedType = getTypeFromCaseInfo caseType
                --             parsedAlts = sequence $ Prelude.zipWith (curry toJava2) alts (createListOfLen alts parsedType)
                --             parsedAlts2 = Prelude.map toJava2 (Prelude.zip alts (createListOfLen alts parsedType))
                --                 where
                --                     createListOfLen :: [a] -> JavaAtom -> [JavaAtom]
                --                     createListOfLen [] a = []
                --                     createListOfLen (x : xs) a = a : createListOfLen xs a

                                    -- asdf :: ToJavaM a -> a
                                    -- asdf (toJava2 x) = x

                            -- cases = Prelude.map toJava2 (return parsedAlts)

            _ ->
                withFreshVars n $ \ xs ->do
                    liftIO do
                        print "function something"
                        putStrLn $ show xs
                        print f
                    -- javaDefine f (dropArgs bs xs) <$> toJava2 body
                    javaDefine f xs <$> toJava2 body

data SpecialCase = BoolCase
isSpecialCase :: CaseInfo -> ToJavaM (Maybe SpecialCase)
isSpecialCase _ = return Nothing

getTypeFromCaseInfo :: CaseInfo -> JavaAtom
getTypeFromCaseInfo (CaseInfo b ct) = case ct of
                                            CTData quan qn -> pack $ prettyShow $ qnameName qn
                                            CTNat -> pack "int"
                                            CTInt -> pack "int"
                                            CTChar -> pack "char"
                                            CTString -> pack "String"
                                            CTFloat -> pack "float"
                                            CTQName -> pack "CTQNameIdkHowToTranslateThisStringIGuess"

        -- withFreshVars n $ \ xs ->
        --     javaDefine f (dropArgs bs xs) <$> toJava2 body

-- instance ToJava2 (Int, [Bool], JavaAtom, TTerm) JavaClass where


instance ToJava2 TTerm JavaBlock where
    toJava2 v = case v of
        TVar n -> do
            liftIO do
                putStrLn "TVar"
            x <- getVar n
            return x
            -- return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        TPrim tp -> return $ Block []
        TDef qn -> return $ Block []
        TApp tt tts -> return $ Block []
        TLam tt -> return $ Block []
        TLit lit -> return $ Block []
        -- TCon qn -> return $ Block []
        TLet tt tt' -> return $ Block []
        TCase num caseType term alts -> do
                special <- isSpecialCase caseType
                case special of
                    -- Nothing | [TACon c nargs v] <- alts -> do
                    --     withFreshVars' EagerEvaluation nargs $ \xs -> do
                    --         let parsedType = getTypeFromCaseInfo caseType
                    --         y <- mapM toJava2 (Prelude.zip alts (createListOfLen alts parsedType))
                    --         -- x <-  Prelude.zipWith (curry toJava2) alts (createListOfLen alts parsedType)
                    --         -- return $ javaUseConstructor (pack $ prettyShow $ qnameName c) f xs
                    --         return $ javaCaseCreate f [parsedType] y Nothing
                    --             where
                    --                 createListOfLen :: [a] -> JavaAtom -> [JavaAtom]
                    --                 createListOfLen [] a = []
                    --                 createListOfLen (x : xs) a = a : createListOfLen xs a
                    Nothing -> do
                        withFreshVars' EagerEvaluation 0 $ \xs -> do
                            let parsedType = getTypeFromCaseInfo caseType
                            y <- mapM toJava2 (Prelude.zip alts (createListOfLen alts parsedType))
                            -- x <-  Prelude.zipWith (curry toJava2) alts (createListOfLen alts parsedType)
                            -- return $ javaUseConstructor (pack $ prettyShow $ qnameName c) f xs
                            -- return $ javaCaseCreate f [] y Nothing
                            return $ Block [BlockStmt $ Return (Just $ MethodInv $ PrimaryMethodCall (ExpName $ Language.Java.Syntax.Name [Ident "b"]) [] (Ident "match") [
                                    InstanceCreation
                                        []
                                        (TypeDeclSpecifier $ ClassType [(Ident $ unpack parsedType ++ ".Visitor", [makeTypeArgument ""])])
                                        []
                                        (Just $ ClassBody y)
                                ] )]
                                where
                                    createListOfLen :: [a] -> JavaAtom -> [JavaAtom]
                                    createListOfLen [] a = []
                                    createListOfLen (x : xs) a = a : createListOfLen xs a
                    Just BoolCase -> __IMPOSSIBLE__
        -- TPrim tp -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show tp]]
        -- TDef qn -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show qn]]
        -- TApp n tts -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TLam n -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TLit n -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TCon n -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ prettyShow $ qnameName n]]
        TCon n -> do
            return $ Block [
                BlockStmt $
                    Return (
                        Just $
                            InstanceCreation
                                []
                                (TypeDeclSpecifier $ ClassType [(Ident $ prettyShow $ qnameName n, [])])
                                []
                                Nothing
                        )
                ]
        -- TLet n tt' -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        -- TCase n ci tt tas -> return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]

        TUnit -> return $ Block []
        TSort -> return $ Block []
        TErased -> return $ Block []
        TCoerce tt -> return $ Block []
        TError te -> return $ Block []
