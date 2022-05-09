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

type JavaAtom = Text
type JavaForm = Decl
type JavaBlock = Block

buildBasicJava :: [Decl] -> CompilationUnit
buildBasicJava xs = CompilationUnit Nothing [] [ClassTypeDecl (ClassDecl [] (Ident "Main") [] Nothing [] (ClassBody xs))]

buildMainMethod :: Maybe Block -> Decl
buildMainMethod block = MemberDecl (MethodDecl [Public, Static] [] Nothing (Ident "main") [FormalParam [] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String", [])]))))) False (VarId (Ident "args"))] [] Nothing (MethodBody block))

buildMainMethodMonad :: [Decl] -> ToJavaM CompilationUnit
buildMainMethodMonad b = do
    -- let intermediate x = buildBasicJava  [buildMainMethod(Just x)]
    return $ buildBasicJava  $ buildMainMethod Nothing : b

-- doesn't work properly yet
makeJavaName :: QName -> ToJavaM JavaAtom
makeJavaName n = return $ pack $ getName n
    where
        getName :: QName -> String
        getName = show

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

defToTreeless2 :: Definition -> ToJavaM (Maybe (Int, [Bool], JavaAtom, TTerm))
defToTreeless2 def
    | defNoCompilation def ||
    not (usableModality $ getModality def) = return Nothing
    | otherwise = do
        let f = defName def
        -- reportSDoc "toJava2" 5 $ "Compiling definition:" <> prettyTCM f
        case theDef def of
            Axiom{} -> do
                f' <- newJavaDef2 f 0 []
                return Nothing
            DataOrRecSig n -> __IMPOSSIBLE__
            GeneralizableVar -> return Nothing
            AbstractDefn de -> __IMPOSSIBLE__
            d@Function {} | d ^. funInline -> return Nothing
            Function cls m_cc m_st m_com cls' fi m_qns ia de m_pro set m_b
                        m_eli m_qn
                -> do
                    strat <- getEvaluationStrategy
                    maybeCompiled <- liftTCM $ toTreeless strat f
                    case maybeCompiled of
                        Nothing -> return Nothing
                        Just body -> do
                            let (n, body') = lambdaView body
                            f' <- newJavaDef2 f n (Prelude.take n [])
                            return $ Just (n,[], f', body')
            Datatype n i m_cl qns so m_qns ia qns' -> return Nothing
            Record n m_cl ch b dos te m_qns ee poc m_in ia ck -> return Nothing
            Constructor {conSrcCon = chead, conArity = nargs} -> return Nothing
            Primitive ia s cls fi m_cc -> do
                f' <- newJavaDef2 f 0 []
                return Nothing
            PrimitiveSort s so -> return Nothing

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

instance ToJava2 (Int, [Bool], JavaAtom, TTerm) JavaForm where
    toJava2 (n, bs, f, body) =
        withFreshVars n $ \xs ->
            javaDefine f (dropArgs bs xs) <$> toJava2 body

instance ToJava2 TTerm JavaBlock where
    toJava2 v = case v of
        TVar n -> do
            x <- getVar n
            return $ Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident $ show n]]
        TPrim tp -> return $ Block []
        TDef qn -> return $ Block []
        TApp tt tts -> return $ Block []
        TLam tt -> return $ Block []
        TLit lit -> return $ Block []
        TCon qn -> return $ Block []
        TLet tt tt' -> return $ Block []
        TCase n ci tt tas -> return $ Block []
        TUnit -> return $ Block []
        TSort -> return $ Block []
        TErased -> return $ Block []
        TCoerce tt -> return $ Block []
        TError te -> return $ Block []
