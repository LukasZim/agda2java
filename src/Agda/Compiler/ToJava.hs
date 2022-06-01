{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
module Agda.Compiler.ToJava where
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
           conSrcCon, conArity, dataCons, recFields, recConHead),
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
import Data.Char

type JavaAtom = Text
type JavaForm = Decl
type JavaBlock = Block
type JavaClass = ClassDecl

type ToJavaM a = StateT ToJavaState (ReaderT ToJavaEnv TCM) a

data ToJavaCon = ToJavaCon JavaAtom Int Bool
data ToJavaDef = ToJavaDef JavaAtom Int [Bool]

initToJavaState :: ToJavaState
initToJavaState = ToJavaState
    {   toJavaFresh = freshVars
        , toJavaDefs = Map.empty
        , toJavaCons = Map.empty
        , toJavaUsedNames = reservedNames
    }

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
        , toJavaVars :: [JavaBlock] -- not sure this is correctly typed yet
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
            Function {} -> do
                strat <- getEvaluationStrategy
                maybeCompiled <- liftTCM $ toTreeless strat f
                case maybeCompiled of
                    Just body -> do
                        let (n, body') = lambdaView body
                        f' <- newJavaDef f n (Prelude.take n (repeat False))
                        return $ Just (n, Prelude.take n (repeat False), f', body', [])
                    Nothing -> return Nothing
            Primitive {} -> do
                f' <- newJavaDef f 0 []
                return Nothing
            PrimitiveSort {} -> return Nothing
            Datatype {dataCons = cs } -> do
                let eraseTag = Prelude.length cs == 1
                forM_ cs $ \c -> do
                    cdef <- theDef <$> getConstInfo c
                    case cdef of
                        Constructor { conSrcCon = chead, conArity = nargs } -> processCon chead nargs eraseTag
                        _ -> __IMPOSSIBLE__
                return Nothing
            Record{ recConHead = chead, recFields = fs } -> do
                processCon chead (Prelude.length fs) True
                return Nothing
            Constructor{} -> return Nothing
            AbstractDefn{} -> __IMPOSSIBLE__
            DataOrRecSig{} -> __IMPOSSIBLE__
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

newJavaDef :: QName -> Int -> [Bool] -> ToJavaM JavaAtom
newJavaDef n i bs = do
    a <- makeJavaName n
    setJavaDef n (ToJavaDef a i bs)
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

isValidJavaChar :: Char -> Bool
isValidJavaChar x 
    | isAscii x = isAlphaNum x
    | otherwise = False

fourBitsToChar :: Int -> Char
fourBitsToChar i = "0123456789ABCDEF" !! i
{-# INLINE fourBitsToChar #-}

isNameUsed :: JavaAtom -> ToJavaM Bool
isNameUsed x = Set.member x <$> gets toJavaUsedNames

setJavaDef :: QName -> ToJavaDef -> ToJavaM ()
setJavaDef n def = do
    modify $ \s -> s {toJavaDefs = Map.insert n def (toJavaDefs s)}

setJavaCon :: QName -> ToJavaCon -> ToJavaM ()
setJavaCon n con = do
    modify $ \s -> s {toJavaCons = Map.insert n con (toJavaCons s)}    

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


instance ToJava (Int, [Bool], JavaAtom, TTerm, [QName]) JavaForm where
    toJava (n, bs, f, body, names) = 
        withFreshVars n $ \xs ->
            javaDefine f xs <$> toJava body

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
    local (addBinding $ forceIfLazy strat (Block [BlockStmt $ ExpStmt $ ExpName $ Language.Java.Syntax.Name [Ident (unpack x)]])) $ f x

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
