module Agda.Compiler.Compiler where
import Agda.Compiler.Backend
import Agda.Compiler.Options
import Agda.Compiler.Syntax (JavaEntry, JavaModule)
import Agda.Interaction.Options (OptDescr)
import Agda.Compiler.Common (compileDir)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Java.Pretty

import Data.Text (Text, pack)
import Control.Monad.IO.Class
import Agda.Utils.Pretty (prettyShow)
import Agda.Compiler.ToJava
import Language.Java.Syntax (CompilationUnit, Block (Block))
import Data.Maybe


test :: String
test = "lumfao"

javaBackend :: Backend
javaBackend = Backend javaBackend'

-- javaBackend' :: Backend' JavaOptions JavaOptions JavaEnv JavaModule [JavaEntry]
javaBackend' :: Backend' JavaOptions JavaOptions () () Definition
javaBackend' = Backend' {
    backendName = "agda2java",
    backendVersion = Nothing,
    options = defaultJavaOptions,
    commandLineFlags = javaCommandLineFlags,
    isEnabled = const True,
    preCompile = javaPreCompile,
    postCompile = \_ _ _ -> return (),
    preModule = \_ _ _ _ -> return $ Recompile (),
    postModule = \ _ _ -> javaPostModule,
    -- compileDef = javaCompileDef,
    compileDef = \ _ _ isMain def -> return def,
    scopeCheckingSuffices = False,
    mayEraseType = const $ return True
    }

javaCommandLineFlags :: [OptDescr (Flag JavaOptions)]
javaCommandLineFlags = []

javaPreCompile :: JavaOptions -> TCM JavaOptions
javaPreCompile = return

-- change javaEntry to something from the java-lang library, that makes it so i don't have to do the pretty printing myself
javaPostModule :: IsMain -> ModuleName -> [Definition] -> TCM ()
javaPostModule isMain modName defs = do
    modtext <- do
        let treelessdefs = map (defToTreeless []) defs
            text = concatMap fst treelessdefs
            d = catMaybes $ map snd treelessdefs
            ds = concatMap (`toJava` text) d
        return $ buildBasicJava [buildMainMethod $ Just $ Block ds]
    let defToText = T.pack . prettyPrint
        -- modText = T.intercalate (pack "\n\n") $ map defToText modtext
        fileText = defToText modtext
        filename = prettyShow (last $ mnameToList modName) ++ ".java"
    -- modText <- runToJavaM opts $ do
    --     xs <- traverse defToTreeless defs
    --     mainMethod <- buildMainMethod mainBlock
    --     buildBasicJava []
    liftIO $ T.writeFile filename fileText


    -- modules' <- traverse (uncurry writeIntermediate) $ Map.toList modules
-- javaCompileDef :: JavaOptions -> () -> IsMain -> Definition -> TCM [CompilationUnit]
-- javaCompileDef _ _ _ def = do
--     toJava def



-- class ToJava a b where
--     toJava :: a -> TCM b

-- instance ToJava Definition [CompilationUnit] where
--     -- toJava def
--     --     | defNoCompilation def || not (usableModality $ getModality def) = return []
--     toJava def = do
--         let qn = defName def
--         case theDef def of
--           Axiom b -> return []
--           DataOrRecSig n -> return []
--           GeneralizableVar -> return []
--           AbstractDefn de -> return []
--           Function cls m_cc m_st m_com cls' fi m_qns ia de m_pro set m_b
--                    m_eli m_qn
--             -> return []
--           Datatype n i m_cl qns so m_qns ia qns' -> return []
--           Record n m_cl ch b dos te m_qns ee poc m_in ia ck -> return []
--           Constructor n i ch qn' ia in' ck m_qns ifs m_bs -> return []
--           Primitive ia s cls fi m_cc -> return []
--           PrimitiveSort s so -> return []
            