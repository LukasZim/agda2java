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
      HasConstInfo(getConstInfo), tAppView, qnameToList )
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
import qualified Data.Foldable as Map
-- import Agda.Compiler.ToJavaBackup (javaDataType, JavaAtom)

type JavaAtom = Text
type JavaForm = Decl
type JavaBlock = BlockStmt
type JavaClass = ClassDecl

type JavaStmt = Decl
type JavaExp = Exp

type ToJavaM a = StateT ToJavaState (ReaderT ToJavaEnv TCM) a

data ToJavaCon = ToJavaCon JavaAtom Int Bool
data ToJavaFun = ToJavaFun JavaAtom Int [Bool]
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
        , toJavaVars :: [JavaBlock] -- not sure this is correctly typed yet
    }

freshVars :: [JavaAtom]
freshVars = Prelude.concat [ Prelude.map (<> i) xs | i <- pack "":Prelude.map (pack . show) [1..]]
    where
        xs = Prelude.map Data.Text.singleton ['a' .. 'z']

buildMainMethodMonad :: [JavaStmt] -> ToJavaM CompilationUnit
buildMainMethodMonad b = do
    -- let intermediate x = buildBasicJava  [buildMainMethod(Just x)]
    return $ buildBasicJava (buildMainMethod (Just $ Block []) : b)

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
        case theDef def of
            Axiom {} -> do
                f' <- newJavaDef f 0 []
                return Nothing
            GeneralizableVar{} -> return Nothing
            d@Function{} | d ^. funInline -> return Nothing
            d@Function {} -> do
                liftIO do
                    putStrLn $ Agda.Compiler.MAlonzo.Pretty.prettyPrint $ qnameName f
                    -- print d
                strat <- getEvaluationStrategy
                maybeCompiled <- liftTCM $ toTreeless strat f
                case maybeCompiled of
                    Just body -> do
                        let (n, body') = lambdaView body
                        f' <- newJavaFun f n (Prelude.take n (Prelude.repeat False))
                        return $ Just (n, Prelude.take n (Prelude.repeat False), f', body', [])
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
                            -- addJavaConToDatatype f name nargs
                        _ -> __IMPOSSIBLE__
                return $ Just (0, [], pack $ Agda.Compiler.MAlonzo.Pretty.prettyPrint $qnameName f, TDef f, [])
            -- Record{ recConHead = chead, recFields = fs } -> do
            --     processCon chead (Prelude.length fs) True
            --     return Nothing
            Record {} -> __IMPOSSIBLE__

            -- vgm hoef je hiero niiks te returnen omdat het al gedaan wordt in datatype
            -- daarom kan je dus ook de constructors maken in de  method die de datatype maakt in
            -- tojava zegmaaar, misschine ook niet want mijn brein loopt momenteel mijn neus uit
            -- en ik wil momenteel even niet bestaan ha ha ha hi ha hondenlul
            Constructor{} -> return Nothing
            AbstractDefn{} -> __IMPOSSIBLE__
            DataOrRecSig{} -> __IMPOSSIBLE__
            where
                processCon :: ConHead -> Int -> Bool -> QName -> ToJavaM JavaAtom
                processCon chead nargs b dataname= do
                    newJavaCon (conName chead) dataname nargs b

                -- getInfo :: QName ->  (String, Int)
                -- getInfo name = do
                --     c <- getConstInfo name
                --     case c of
                --         -- Constructor n i ch qn ia in' ck m_qns ifs m_bs ->  (show $qnameName name, n)
                --         -- _ -> __IMPOSSIBLE__
                --     -- return c



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

newJavaFun :: QName -> Int -> [Bool] -> ToJavaM JavaAtom
newJavaFun n i bs = do
    a <- makeJavaName n
    setJavaFun n (ToJavaFun a i bs)
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



instance ToJava (Int, [Bool], JavaAtom, TTerm, [QName]) [JavaStmt] where
    toJava (n, bs, f, body, names) = case body of
        TDef d ->  do
            --hier zou je oook de constructors dierct kunnen maken
            -- vgm kan je op 1of andere manier een lookup doen?
            ToJavaDef d' i bs visitor <- lookupJavaDef d
            constructors <- mapM toJava bs
            return $ buildJavaDefinition d' i bs visitor
            -- return $ BlockStmt Empty
        _ -> __IMPOSSIBLE__
        -- _ ->    withFreshVars n $ \ xs ->do
        --             liftIO do
        --                 print "function something"
        --                 putStrLn $ show xs
        --                 print f
        --             javaDefine f xs <$> toJava body

instance ToJava TTerm JavaBlock where
    toJava n = __IMPOSSIBLE__

instance ToJava (JavaAtom , (String, Int)) JavaStmt where
    toJava (datatype, (name, nargs)) = return $ buildJavaConstructor datatype (name, nargs)

buildJavaConstructor :: JavaAtom -> (String, Int) -> JavaStmt
buildJavaConstructor datatype (name , nargs) = MemberDecl $ MemberClassDecl $
    ClassDecl
        [Static]
        (Ident name)
        []
        (Just $ ClassRefType $ ClassType [(Ident $ unpack datatype, [])])
        []
        (ClassBody [])

typeAgda :: Language.Java.Syntax.Type
typeAgda = RefType $ ClassRefType $ ClassType [(Ident "Agda", [])]

buildClassBody :: (String, Int) -> [JavaStmt]
buildClassBody (name, nargs) = buildPrivateFields nargs ++ [buildConstructorConstructor name nargs, (buildMatchFunction)]
    where
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
        buildParams n = FormalParam [] typeAgda False (VarId $ Ident ("arg" + show n)) : buildParams (n - 1)

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
buildJavaAbstractClass name = MemberDecl $ MemberClassDecl $ ClassDecl [Abstract, Static] (Ident $ unpack name) [] (Just $ ClassRefType $ ClassType [(Ident "AgdaData", [])]) [] (ClassBody [])
    -- build visitor
    -- build the static class




javaApps :: JavaBlock -> [JavaBlock] -> JavaBlock
-- build this : ((AgdaData) b1).match(andF);
javaApps f args = Prelude.foldl (\x y -> x) f args

getVar :: Int -> ToJavaM JavaBlock
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
    local (addBinding $ forceIfLazy strat BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident (unpack x)]) $ f x

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
