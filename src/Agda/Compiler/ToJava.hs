{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
module Agda.Compiler.ToJava where
import Agda.Compiler.Syntax
import Agda.Compiler.Backend
    ( EvaluationStrategy(EagerEvaluation),
      toTreeless,
      funInline,
      Name (nameBindingSite),
      QName(qnameName, QName),
      CaseInfo(CaseInfo),
      CaseType(CTQName, CTData, CTNat, CTInt, CTChar, CTString, CTFloat),
      TAlt(TALit, TACon, TAGuard),
      TTerm(..),
      Definition(defNoCompilation, defName, theDef),
      Defn(PrimitiveSort, Axiom, DataOrRecSig, GeneralizableVar,
           AbstractDefn, Function, Datatype, Record, Constructor, Primitive,
           conSrcCon, conArity, dataCons, recFields, recConHead),
      MonadTCM(liftTCM),
      TCM,
      HasConstInfo(getConstInfo), tAppView, qnameToList, lookupDefinition, TPrim (PSeq) )
import Agda.Syntax.Common
import Control.Monad.State
import Language.Java.Syntax
import Agda.Compiler.Treeless.Erase
import Data.Text
import Data.Map
import Data.Set
import Agda.Utils.List1
import Agda.Utils.Pretty (prettyShow)

import Language.Java.Pretty

import Agda.Syntax.Internal (ConHead(conName), Type, Term (Var))
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
import Data.Char
import Agda.Compiler.Treeless.EliminateLiteralPatterns (eliminateLiteralPatterns)
import Agda.Compiler.Treeless.GuardsToPrims (convertGuards)
import Distribution.SPDX (LicenseId(AFL_3_0))

type JavaAtom = Text
type JavaForm = Decl
type JavaBlock = BlockStmt
type JavaClass = ClassDecl

type JavaStmt = Decl
type JavaExp = Exp

type ToJavaM a = StateT ToJavaState (ReaderT ToJavaEnv TCM) a

data ToJavaCon = ToJavaCon JavaAtom Int Bool
data ToJavaFun = ToJavaFun JavaAtom Int [Bool] TTerm
data ToJavaDef = ToJavaDef JavaAtom Int [(String, Int)] JavaAtom

initToJavaState :: ToJavaState
initToJavaState = ToJavaState
    {   toJavaFresh = freshVars
        , toJavaFuns = Map.empty
        , toJavaDefs = Map.empty
        , toJavaCons = Map.empty
        , toJavaUsedNames = reservedNames
    }

data ToJavaState = ToJavaState
    {
    toJavaFresh :: [JavaAtom]
    , toJavaFuns :: Map QName ToJavaFun
    , toJavaDefs :: Map QName ToJavaDef
    , toJavaCons :: Map QName ToJavaCon
    , toJavaUsedNames :: Set JavaAtom
    }

data ToJavaEnv = ToJavaEnv
    {
        toJavaOptions :: JavaOptions
        , toJavaVars :: [JavaExp]
    }

freshVars :: [JavaAtom]
freshVars = Prelude.concat [ Prelude.map (<> i) xs | i <- pack "":Prelude.map (pack . show) [1..]]
    where
        xs = Prelude.map Data.Text.singleton ['a' .. 'z']

buildMainMethodMonad :: [BlockStmt] -> [JavaStmt] -> ToJavaM CompilationUnit
buildMainMethodMonad block b = do
    return $ buildBasicJava (buildMainMethod (Just $ Block block) : b)

buildBasicJava :: [Decl] -> CompilationUnit
buildBasicJava xs = CompilationUnit Nothing [] [ClassTypeDecl (ClassDecl [] (Ident "Main") [] Nothing [] (ClassBody xs))]

buildMainMethod :: Maybe Block -> Decl
buildMainMethod block = MemberDecl (MethodDecl [Public, Static] [] Nothing (Ident "main") [FormalParam [] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String", [])]))))) False (VarId (Ident "args"))] [] Nothing (MethodBody block))


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

runToJavaM :: JavaOptions -> ToJavaM a -> TCM a
runToJavaM opts =
      (`runReaderT` initToJavaEnv opts)
    . (`evalStateT` initToJavaState)

initToJavaEnv :: JavaOptions -> ToJavaEnv
initToJavaEnv opts = ToJavaEnv opts []

defToTreeless :: Definition -> ToJavaM (Maybe (Int, [Bool], JavaAtom, TTerm, [QName]))
defToTreeless def
    | defNoCompilation def ||
    not (usableModality $ getModality def) = return Nothing
    | otherwise = do
        let f = defName def
            c = theDef def
        case c of
            Axiom {} -> do
                return Nothing
            GeneralizableVar{} -> return Nothing
            d@Function{} | d ^. funInline -> return Nothing
            d@Function {} -> do
                strat <- getEvaluationStrategy
                maybeCompiled <- liftTCM $ toTreeless strat f
                case maybeCompiled of
                    Just body -> do
                        let (n, body') = lambdaView body
                        f' <- newJavaFun f n (Prelude.take n (Prelude.repeat False)) body
                        return $ Just (n, Prelude.take n (Prelude.repeat False), f', TDef f, [])
                    Nothing -> return Nothing
            Primitive {} -> do
                f' <- newJavaDef f 0 []
                return Nothing
            PrimitiveSort {} -> return Nothing
            Datatype {dataCons = cs } -> do
                let eraseTag = Prelude.length cs == 1
                f' <- newJavaDef f 0 []
                forM_ cs $ \c -> do
                    cdef <- theDef <$> getConstInfo c
                    case cdef of
                        Constructor { conSrcCon = chead, conArity = nargs } -> do
                            processCon chead nargs eraseTag f
                            lookupJavaDef f
                        _ -> __IMPOSSIBLE__
                return $ Just (0, [], pack $ Agda.Compiler.MAlonzo.Pretty.prettyPrint $qnameName f, TCon f, [])
            Record {} -> __IMPOSSIBLE__

            Constructor{} -> return Nothing
            AbstractDefn{} -> __IMPOSSIBLE__
            DataOrRecSig{} -> __IMPOSSIBLE__
            where
                processCon :: ConHead -> Int -> Bool -> QName -> ToJavaM JavaAtom
                processCon chead nargs b dataname= do
                    newJavaCon (conName chead) dataname nargs b


newJavaCon :: QName -> QName -> Int -> Bool -> ToJavaM JavaAtom
newJavaCon n dataname i b = do
    a <- makeJavaName n
    setJavaCon n (ToJavaCon a i b)
    modify $ \s -> s {toJavaCons = toJavaCons s}
    addJavaCon (unpack a, i) dataname
    modify $ \s -> s {toJavaCons = toJavaCons s}
    setNameUsed a
    return a

newJavaDef :: QName -> Int -> [(String, Int)] -> ToJavaM JavaAtom
newJavaDef n i bs = do
    a <- makeJavaName n
    let newName = unpack a Prelude.++ "Visitor"
    b <- makeVisitorName newName

    setJavaDef n (ToJavaDef a i bs b)
    setNameUsed a
    return a

newJavaFun :: QName -> Int -> [Bool] -> TTerm -> ToJavaM JavaAtom
newJavaFun n i bs body = do
    a <- makeJavaName n
    setJavaFun n (ToJavaFun a i bs body)
    setNameUsed a
    return a

makeJavaName :: QName -> ToJavaM JavaAtom
makeJavaName n = go $ fixName $ prettyShow $ qnameName n
    where
        nextName = ('z':)
        go s = ifM (isNameUsed $ T.pack s) (go $ nextName s) (return $ T.pack s)

        fixName s =
            let s' = Prelude.concat (Prelude.map fixChar s) in
                if isNumber (Prelude.head s') then "z" ++ s' else s'

        fixChar c
            | isValidJavaChar c = [c]
            | otherwise         = "\\x" ++ toHex (ord c) ++ ";"

        toHex 0 = ""
        toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]

makeVisitorName :: String -> ToJavaM JavaAtom
makeVisitorName n = go $ fixName n
    where
        nextName = ('z':)
        go s = ifM (isNameUsed $ T.pack s) (go $ nextName s) (return $ T.pack s)

        fixName s =
            let s' = Prelude.concat (Prelude.map fixChar s) in
                if isNumber (Prelude.head s') then "z" ++ s' else s'

        fixChar c
            | isValidJavaChar c = [c]
            | otherwise         = "\\x" ++ toHex (ord c) ++ ";"

        toHex 0 = ""
        toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]

isValidJavaChar :: Char -> Bool
isValidJavaChar x
    | isAscii x = isAlphaNum x
    | otherwise = False

fourBitsToChar :: Int -> Char
fourBitsToChar i = "0123456789ABCDEF" Prelude.!! i
{-# INLINE fourBitsToChar #-}

isNameUsed :: JavaAtom -> ToJavaM Bool
isNameUsed x = Set.member x <$> gets toJavaUsedNames


setJavaDef :: QName -> ToJavaDef -> ToJavaM ()
setJavaDef n def = do
    modify $ \s -> s {toJavaDefs = Map.insert n def (toJavaDefs s)}

setJavaFun :: QName -> ToJavaFun -> ToJavaM ()
setJavaFun n def = do
    modify $ \s -> s {toJavaFuns = Map.insert n def (toJavaFuns s)}

setJavaCon :: QName -> ToJavaCon -> ToJavaM ()
setJavaCon n con = do
    modify $ \s -> s {toJavaCons = Map.insert n con (toJavaCons s)}


addJavaCon :: (String, Int) -> QName -> ToJavaM ()
addJavaCon (str, num) name = do
    modify \s -> s {toJavaDefs = Map.adjustWithKey (f (str, num) ) name (toJavaDefs s)}



f :: (String, Int) -> QName -> ToJavaDef ->  ToJavaDef
f x key value = case value of { ToJavaDef txt i xs visitor -> ToJavaDef txt i (x : xs) visitor}




setNameUsed :: JavaAtom -> ToJavaM ()
setNameUsed x = modify \s ->
    s {toJavaUsedNames = Set.insert x (toJavaUsedNames s)}


getEvaluationStrategy :: ToJavaM EvaluationStrategy
getEvaluationStrategy = reader $ javaEvaluation . toJavaOptions

lambdaView :: TTerm -> (Int, TTerm)
lambdaView v = case v of
  TLam    w -> first (1+) $ lambdaView w
  TCoerce w -> lambdaView w
  _         -> (0, v)


lookupJavaDef :: QName -> ToJavaM ToJavaDef
lookupJavaDef n = do
    let defs = gets toJavaDefs
    r <- Map.lookup n <$> defs
    case r of
        Nothing -> fail "couldn't find definition"
        Just a -> return a

lookupJavaFun :: QName -> ToJavaM ToJavaFun
lookupJavaFun n = do
    let defs = gets toJavaFuns
    r <- Map.lookup n <$> defs
    case r of
        Nothing -> fail "couldn't find definition"
        Just a -> return a

getJavaFunctionNames :: ToJavaM [JavaAtom]
getJavaFunctionNames = do
    let defs = gets toJavaFuns
    res <- Map.toList <$> defs
    let res2 = helper res
    return res2
        where
            helper :: [(QName, ToJavaFun)]  -> [JavaAtom]
            helper [] = []
            helper ((qname, ToJavaFun name _ _ _):xs) = name : helper xs

createFields :: [JavaAtom] -> [Decl]
createFields xs = [MemberDecl $ FieldDecl [Private, Static] (makeType "Agda") (createDecl123 xs)]
    where
        createDecl123 :: [JavaAtom] -> [VarDecl]
        createDecl123 [] = []
        createDecl123 (x:xs) = VarDecl (VarId $ Ident $unpack x) Nothing : createDecl123 xs

lookupJavaCon :: QName -> ToJavaM ToJavaCon
lookupJavaCon n = do
    let defs = gets toJavaCons
    r <- Map.lookup n <$> defs
    case r of
        Nothing -> fail "couldn't find definition"
        Just a -> return a


instance ToJava (TAlt, [JavaAtom], Integer) JavaStmt where
    toJava (n, xs, index) = case n of
      TACon qname nargs body -> do
            ToJavaCon name nargs bool <- lookupJavaCon qname
        --   withFreshVar $ \id ->
            withFreshVars nargs $ \vars -> do
                -- this call is funky apparently
                Block body' <- toJava (body, xs, index)
                body'' <- buildTAConBody body' name vars
                return $ MemberDecl
                    (MethodDecl
                        [Public]
                        []
                        (Just typeAgda)
                        (Ident $unpack name)
                        (buildParamsFromVars vars)
                        []
                        Nothing
                        (MethodBody $ Just body''))
      TAGuard tt tt' -> __IMPOSSIBLE__
      TALit lit tt -> __IMPOSSIBLE__

buildTAConBody :: [BlockStmt] -> JavaAtom -> [JavaAtom] -> ToJavaM Block
buildTAConBody body constName vars = do
    body' <- buildReturnStatement body
    return $ Block body'
            where
                constArgs :: [JavaAtom] -> [Exp]
                constArgs [] = []
                constArgs (x:xs) = ExpName (Name [Ident $ unpack x]) : constArgs xs

                buildReturnStatement :: [BlockStmt] -> ToJavaM [BlockStmt]
                buildReturnStatement [] = __IMPOSSIBLE__
                buildReturnStatement [stmt] = case stmt of
                  BlockStmt  (Return x) -> return  [BlockStmt $ Return x]
                  BlockStmt (ExpStmt exp) -> return  [BlockStmt $ Return (Just exp)]
                  x ->do
                    __IMPOSSIBLE__
                buildReturnStatement (x:xs) = buildReturnStatement xs


buildParamsFromVars :: [JavaAtom] -> [FormalParam]
buildParamsFromVars = Prelude.map
      (\ n -> FormalParam [] typeAgda False (VarId $ Ident $ unpack n))



instance ToJava (Int, [Bool], JavaAtom, TTerm, [QName]) [JavaStmt] where
    toJava (n, bs, f, body, names) = case body of
        TCon d ->  do
            ToJavaDef d' i bs visitor <- lookupJavaDef d
            constructors <- mapM toJava (Prelude.zip (Prelude.replicate (Prelude.length bs) (d', visitor)) bs)
            return $ buildJavaDefinition d' i bs visitor ++ constructors
            -- return $ BlockStmt Empty
        TVar {} -> __IMPOSSIBLE__
        TPrim {} -> __IMPOSSIBLE__
        TApp {} -> __IMPOSSIBLE__
        TLam {} ->  __IMPOSSIBLE__
        TLit {} -> __IMPOSSIBLE__
        TDef defName -> do
            ToJavaFun name num bs body <- lookupJavaFun defName
            x <- withFreshVars num \xs -> do
                let zero :: Int
                    zero = 0
                parsedBody <- toJava (body, xs, zero)
                return $ javaDefine name xs parsedBody
            return [x]
            -- return []
        TLet {} ->  __IMPOSSIBLE__
        TCase {} ->  __IMPOSSIBLE__
        TUnit ->  __IMPOSSIBLE__
        TSort ->  __IMPOSSIBLE__
        TErased ->  __IMPOSSIBLE__
        TCoerce {} ->  __IMPOSSIBLE__
        TError {} ->  __IMPOSSIBLE__
        -- _ -> __IMPOSSIBLE__

instance ToJava TTerm JavaExp where
    -- toJava n = __IMPOSSIBLE__
    toJava n = case n of
        cc@(TCase n c t a) -> do
            let zero :: Int
                zero = 0
                empty :: [JavaAtom]
                empty = []
            toJava (cc, empty, zero)
        TCon qname -> do
            ToJavaCon name nargs bool <- lookupJavaCon qname
            return $  InstanceCreation [] (TypeDeclSpecifier $ ClassType ([(Ident $ unpack name, [])])) [] Nothing
        TVar n -> do
            getVar n
        x -> do
            let zero :: Int
                zero = 0
                empty :: [JavaAtom]
                empty = []
            toJava (x, empty, zero)
            -- __IMPOSSIBLE__

instance ToJava (TTerm, [JavaAtom], Integer) Block where
    toJava (n , xs , index) = case n of
        TCase num caseType term alts -> do
            special <- isSpecialCase caseType
            case special of
                Nothing -> do
                    -- withFreshVars' EagerEvaluation (fromInteger index) $ \xss -> do
                        let parsedType = getTypeFromCaseInfo caseType
                        qname <- getConstNameFromCaseInfo caseType
                        case qname of
                            Nothing -> __IMPOSSIBLE__
                            Just qn -> do
                                ToJavaDef atom _ constr visitorName <- lookupJavaDef qn
                                x <- buildCaseClasses atom constr visitorName xs (index+1) alts
                                return $ Block[BlockStmt $ Return $ Just $ MethodInv $
                                    PrimaryMethodCall
                                        (Cast (makeType "AgdaData") (ExpName $ Name [Ident $ unpack (xs Prelude.!! Prelude.fromIntegral num)]))
                                        []
                                        (Ident "match")
                                        [InstanceCreation
                                            []
                                            (TypeDeclSpecifier $ ClassType [(Ident $ unpack visitorName, [])])
                                            []
                                            (Just $ ClassBody x)
                                        ]]
                Just sc -> __IMPOSSIBLE__
        TLet u v -> do
            expr <- toJava u
            withFreshVar $ \x -> do
                Block xs <- toJava (v , x:xs, index)
                -- return $ BlockStmt$ ExpStmt body
                -- return $ Block (LocalVars [] (makeType "Agda") [VarDecl (VarId $ Ident $ unpack x) (Just $InitExp expr)] : xs)
                __IMPOSSIBLE__

        TDef defName -> do
            ToJavaFun name num bs body <- lookupJavaFun defName
            return $ Block[BlockStmt $ ExpStmt $ ExpName $ Name [Ident $ unpack name]]

        x -> do
            y <- toJava x
            return $ Block[BlockStmt$  ExpStmt y]

instance ToJava (TTerm, [JavaAtom], Int) JavaExp where
    toJava (n , xs, index) = case n of
        TLam body -> do

            case body of
                TLam _ -> do
                    parsedBody <- toJava (body, xs, index + 1)
                    return $ buildLambda xs parsedBody index
                _ -> do
                    let zero :: Integer
                        zero = 0
                    Block stmts <- toJava (body, xs, zero)
                    return $ buildLastLambda  xs stmts index

        TCon qname -> do
            ToJavaCon name nargs bool <- lookupJavaCon qname
            return $  InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ unpack name, [])]) [] Nothing
        TApp term args -> case term of
            TDef qname -> do
                ToJavaFun name nargs _ body <- lookupJavaFun qname

                expArgs <- mapM (toJava . helperFun xs index) args
                return $ useRunFunction name  expArgs
            TCon qname ->  do
                ToJavaCon name nargs _ <- lookupJavaCon qname
                expArgs <- mapM (toJava . helperFun xs index) args
                return $ InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ unpack name, [])]) expArgs Nothing
            TVar i -> do
                var <- getVar i
                expArgs <- mapM (toJava . helperFun xs index) args
                case var of
                  ExpName (Name[Ident x]) -> return var
                  _ -> __IMPOSSIBLE__
            TPrim PSeq -> do
                case args of
                  [] -> __IMPOSSIBLE__
                  tt : tts -> do
                    var <- toJava (tt , xs, index)
                    case var of
                        ExpName (Name [Ident name])-> do
                            compiledArgs <- mapM (\x -> toJava(x, xs, index)) tts
                            return $ useRunFunction (pack name) compiledArgs
                        x -> __IMPOSSIBLE__

            x -> do
                __IMPOSSIBLE__
        TDef defName -> do
            ToJavaFun name num bs body <- lookupJavaFun defName
            return $ ExpName $ Name [Ident $ unpack name]
        TErased -> return $ Lit Null

        x  -> do
            toJava x

helperFun :: [JavaAtom] -> Int -> TTerm -> (TTerm, [JavaAtom], Int)
helperFun atoms num term = (term, atoms, num)
instance ToJava ((JavaAtom, JavaAtom) , (String, Int)) JavaStmt where
    toJava ((datatype, visitor), (name, nargs)) = do
        return $ buildJavaConstructor datatype visitor (name, nargs)

useRunFunctionHelper :: JavaAtom -> [Exp] -> JavaExp
useRunFunctionHelper name xs = useRunFunction name  (Prelude.reverse xs)

useRunFunction :: JavaAtom -> [Exp] -> JavaExp
useRunFunction name [] = __IMPOSSIBLE__
useRunFunction name [x] = MethodInv $ MethodCall (Name [Ident "runFunction"]) [x, Cast (makeType "AgdaLambda") $ ExpName $ Name [Ident $ unpack name]]
useRunFunction name (x:xs) =  MethodInv $ MethodCall (Name [Ident "runFunction"]) [x, Cast (makeType "AgdaLambda") $ useRunFunction name xs]

-- dit is waar er momenteel een bug zit
buildCaseClasses :: JavaAtom -> [(String,Int)] ->JavaAtom -> [JavaAtom] -> Integer -> [TAlt] -> ToJavaM [Decl]
buildCaseClasses typeName constructors visitorName xs index alts = do
    mapM (buildCase typeName constructors visitorName xs index) alts




altNargs :: TAlt -> ToJavaM Int
altNargs alt = case alt of
    TACon _ nargs _ -> return nargs
    _ -> __IMPOSSIBLE__

buildCase ::  JavaAtom -> [(String,Int)] ->JavaAtom -> [JavaAtom] -> Integer -> TAlt -> ToJavaM Decl
buildCase typeName constructors visitorName xs index alt = do
    toJava (alt, xs, index)

getTypeFromCaseInfo :: CaseInfo -> JavaAtom
getTypeFromCaseInfo (CaseInfo b ct) = case ct of
                                            CTData quan qn -> pack $ prettyShow $ qnameName qn
                                            CTNat -> pack "int"
                                            CTInt -> pack "int"
                                            CTChar -> pack "char"
                                            CTString -> pack "String"
                                            CTFloat -> pack "float"
                                            CTQName -> pack "ERROR_NOT_SUPPORTED"

getConstNameFromCaseInfo :: CaseInfo -> ToJavaM (Maybe QName)
getConstNameFromCaseInfo (CaseInfo b ct) = case ct of
  CTData quan qn -> return $ Just qn
  _ -> return Nothing

data SpecialCase = BoolCase
isSpecialCase :: CaseInfo -> ToJavaM (Maybe SpecialCase)
isSpecialCase _ = return Nothing


buildLambda :: [JavaAtom] -> JavaExp -> Int ->  JavaExp
buildLambda xs body index = Cast (makeType "AgdaLambda") (Lambda (LambdaSingleParam $ Ident $unpack $  xs Prelude.!! index) (LambdaExpression body))

buildLastLambda :: [JavaAtom] -> [JavaBlock] -> Int -> JavaExp
buildLastLambda xs body index = Cast (makeType "AgdaLambda") (Lambda (LambdaSingleParam $ Ident $unpack $ xs Prelude.!! index) (LambdaBlock $ Block body))

javaDefine :: JavaAtom -> [JavaAtom] -> JavaExp -> JavaStmt
javaDefine name xs body = buildMainMethod (Just $ Block[ BlockStmt $ ExpStmt $ Assign (NameLhs $ Name [Ident $ unpack name]) EqualA body])

buildJavaConstructor :: JavaAtom -> JavaAtom -> (String, Int) -> JavaStmt
buildJavaConstructor datatype visitorName (name , nargs) = MemberDecl $ MemberClassDecl $
    ClassDecl
        [Static]
        (Ident name)
        []
        (Just $ ClassRefType $ ClassType [(Ident $ unpack datatype, [])])
        []
        (ClassBody (buildClassBody visitorName (name, nargs)))

typeAgda :: Language.Java.Syntax.Type
typeAgda = RefType $ ClassRefType $ ClassType [(Ident "Agda", [])]

makeType :: String -> Language.Java.Syntax.Type
makeType name = RefType $ ClassRefType $ ClassType [(Ident name, [])]

buildRunFunction :: JavaStmt
buildRunFunction = MemberDecl $ MethodDecl
    [Public, Static]
    []
    (Just typeAgda)
    (Ident "runFunction")
    [
        FormalParam [] typeAgda False (VarId $ Ident "arg"),
        FormalParam [] (makeType "AgdaLambda") False (VarId $ Ident "l")
    ] --params
    []
    Nothing
    (MethodBody (Just $ Block [
        BlockStmt $ Return (Just $ MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "l"]) [] (Ident "run") [ExpName $ Name [Ident "arg"]])
    ]))

buildClassBody :: JavaAtom -> (String, Int) -> [JavaStmt]
buildClassBody visitorName (name, nargs) = buildPrivateFields nargs ++ [buildConstructorConstructor name nargs, buildMatchFunction name visitorName nargs]
    where
        buildMatchFunction :: String -> JavaAtom -> Int -> JavaStmt
        buildMatchFunction consName visitorName nargs = MemberDecl $
            MethodDecl
                [Public]
                []
                (Just typeAgda)
                (Ident "match")
                [FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident "Visitor", [])]) False (VarId $ Ident "visitor")]
                []
                Nothing
                (MethodBody (Just $ Block [
                    BlockStmt $ Return (
                        Just $ MethodInv $ PrimaryMethodCall (Cast (makeType $unpack  visitorName) (ExpName (Name [Ident "visitor"])) ) [] (Ident consName) (buildArgs nargs)
                    )
                ]))

        buildArgs :: Int -> [Argument]
        buildArgs 0 = []
        buildArgs n = ExpName ( Name [Ident $ "arg" ++ show n]) : buildArgs (n - 1)

        buildPrivateFields :: Int -> [JavaStmt]
        buildPrivateFields 0 = []
        buildPrivateFields n = MemberDecl (
            FieldDecl
                [Private, Final]
                (RefType $ ClassRefType $ ClassType [(Ident "Agda", [])])
                [VarDecl (VarId $ Ident ("arg" ++ show n)) Nothing]) : buildPrivateFields (n - 1)

        buildConstructorConstructor :: String -> Int -> JavaStmt
        buildConstructorConstructor name n = MemberDecl $
            ConstructorDecl
                [Public]
                []
                (Ident name)
                (buildParams n)
                []
                (ConstructorBody Nothing (buildConstructorBody n))

        buildConstructorBody :: Int -> [BlockStmt]
        buildConstructorBody 0 = []
        buildConstructorBody n = BlockStmt (ExpStmt $ Assign (FieldLhs $ PrimaryFieldAccess This (Ident $ "arg" ++ show n)) EqualA (ExpName $ Name [Ident $"arg" ++ show n])) : buildConstructorBody (n - 1)

buildParams :: Int -> [FormalParam]
buildParams 0 = []
buildParams n = FormalParam [] typeAgda False (VarId $ Ident ("arg" ++ show n)) : buildParams (n - 1)

buildJavaDefinition :: JavaAtom -> Int -> [(String, Int)] -> JavaAtom -> [JavaStmt]
buildJavaDefinition name nargs consNames visitorName = [buildJavaVisitor visitorName consNames, buildJavaAbstractClass name]


buildJavaVisitor :: JavaAtom -> [(String, Int)] -> JavaStmt
buildJavaVisitor name constructors = MemberDecl $ MemberInterfaceDecl $ InterfaceDecl InterfaceNormal [] (Ident $unpack name) [] [ClassRefType $ ClassType [(Ident "Visitor" , [])]] (InterfaceBody (Prelude.map buildMethod constructors))
    where
        methods = Prelude.map buildMethod constructors

        buildMethod :: (String, Int) -> MemberDecl
        buildMethod (name, nargs) = MethodDecl [] [] (Just $ RefType $ClassRefType$ ClassType [(Ident "Agda", [])]) (Ident name) (buildParams nargs) [] Nothing (MethodBody Nothing)

        buildParams :: Int -> [FormalParam]
        buildParams 0 = []
        buildParams n = FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident "Agda", [])]) False (VarId $ Ident ("arg" ++ show n)): buildParams (n - 1)

buildJavaAbstractClass :: JavaAtom -> JavaStmt
buildJavaAbstractClass name = MemberDecl $ MemberClassDecl $ ClassDecl [Abstract, Static] (Ident $ unpack name) [] Nothing [ClassRefType $ ClassType [(Ident "AgdaData", [])]] (ClassBody [])

javaApps :: JavaBlock -> [JavaBlock] -> JavaBlock
javaApps f args = Prelude.foldl (\x y -> x) f args

getVar :: Int -> ToJavaM JavaExp
getVar i = reader $ (Prelude.!! i) . toJavaVars

makeDelay :: ToJavaM (JavaExp -> JavaExp)
makeDelay = delayIfLazy <$> getEvaluationStrategy

delayIfLazy :: EvaluationStrategy -> JavaExp -> JavaExp
delayIfLazy strat = id


class ToJava a b | a -> b where
    toJava :: a -> ToJavaM b

withFreshVars :: Int -> ([JavaAtom] -> ToJavaM a) -> ToJavaM a
withFreshVars i f = do
  strat <- getEvaluationStrategy
  withFreshVars' strat i f

withFreshVars' :: EvaluationStrategy -> Int -> ([JavaAtom] -> ToJavaM a) -> ToJavaM a
withFreshVars' strat i f
  | i <= 0    = f []
  | otherwise = withFreshVar' strat $ \x -> withFreshVars' strat (i-1) (f . (x:))

withFreshVar :: (JavaAtom -> ToJavaM a) -> ToJavaM a
withFreshVar f = do
    strat <- getEvaluationStrategy
    withFreshVar' strat f

withFreshVar' :: EvaluationStrategy -> (JavaAtom -> ToJavaM a) -> ToJavaM a
withFreshVar' strat f = do
    x <- freshJavaAtom
    local (addBinding $ forceIfLazy strat ExpName $ Language.Java.Syntax.Name [Ident (unpack x)]) $ f x

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

addBinding :: JavaExp -> ToJavaEnv -> ToJavaEnv
addBinding x env = env { toJavaVars = x : toJavaVars env}


getInside :: JavaStmt -> Maybe [BlockStmt]
getInside (MemberDecl (MethodDecl _ _ _ (Ident "main") _ _ _ (MethodBody (Just ( Block x))))) = Just x
getInside _ = Nothing

getOutside :: JavaStmt -> Maybe JavaStmt
getOutside x = case getInside x of
  Nothing -> Just x
  Just bss -> Nothing

