module Agda.Compiler.Compiler where
import Agda.Compiler.Backend
import Agda.Compiler.Options
import Agda.Compiler.Syntax (JavaEntry, JavaModule)
import Agda.Interaction.Options (OptDescr)
import Agda.Compiler.Common (compileDir)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Java.Pretty

import Data.Text (Text, pack, unpack)
import Control.Monad.IO.Class
import Agda.Utils.Pretty (prettyShow)
import Agda.Compiler.ToJava
import Language.Java.Syntax (CompilationUnit, Block (Block), Modifier (Public))
import Data.Maybe


test :: String
test = "lumfao"

javaBackend :: Backend
javaBackend = Backend javaBackend'

-- javaBackend' :: Backend' JavaOptions JavaOptions JavaEnv JavaModule [JavaEntry]
javaBackend' :: Backend' JavaOptions JavaOptions () () (IsMain, Definition)
javaBackend' = Backend' {
    backendName = "agda2java",
    options = JavaOptions EagerEvaluation,
    commandLineFlags = javaCommandLineFlags,
    isEnabled = const True,
    preCompile = javaPreCompile,
    postCompile = \_ _ _ -> return (),
    preModule = \_ _ _ _ -> return $ Recompile (),
    compileDef = \ _ _ isMain def -> return (isMain, def),
    postModule = javaPostModule,
    -- compileDef = javaCompileDef,
    backendVersion = Nothing,
    scopeCheckingSuffices = False,
    mayEraseType = const $ return True
}

javaCommandLineFlags :: [OptDescr (Flag JavaOptions)]
javaCommandLineFlags = []

javaPreCompile :: JavaOptions -> TCM JavaOptions
javaPreCompile = return

-- change javaEntry to something from the java-lang library, that makes it so i don't have to do the pretty printing myself
-- javaPostModule :: IsMain -> ModuleName -> [(IsMain, Definition)] -> TCM ()
-- javaPostModule isMain modName defs = do
--     modtext <- do
--         --[([Text], Maybe TTerm)] ->  ([Text], [Maybe TTerm])
--         let treelessdefs = traverse (defToTreeless [] . snd) defs
--             t1 = fst treelessdefs
--             -- t2 = map pack $ concat $ map (map unpack) t1
--             -- text = concatMap fst treelessdefs
--             t = map snd defs
--             ts = traverse (defToTreeless []) t

--             d = catMaybes $ snd treelessdefs
--             ds = concat $ traverse (`toJava` t1) d
--         -- tts <- traverse (defToTreeless [] (map snd defs))
--         -- dds <- traverse (`toJava` []) ts
--         dds <- traverse toJava2 ts
--         return $ buildBasicJava [buildMainMethod $ Just $ Block ds]
--     let defToText = T.pack . prettyPrint
--         -- modText = T.intercalate (pack "\n\n") $ map defToText modtext
--         fileText = defToText modtext
--         filename = prettyShow (last $ mnameToList modName) ++ ".java"
--     -- modText <- runToJavaM opts $ do
--     --     xs <- traverse defToTreeless defs
--     --     mainMethod <- buildMainMethod mainBlock
--     --     buildBasicJava []
--     liftIO $ T.writeFile filename fileText

javaPostModule :: JavaOptions -> () -> IsMain -> ModuleName -> [(IsMain, Definition)] -> TCM ()
javaPostModule opts _ ismain modName defs = do
    let defToText :: CompilationUnit -> Text
        defToText = T.pack . prettyPrint
        fileName = prettyShow (last $ mnameToList modName) ++ ".java"
    liftIO do
        buildMethodIO [] Nothing "fuckHarrie" [("object" , [], "harrie")] Nothing [Public]
        

    modText <- runToJavaM opts $ do
        ts <- catMaybes <$> traverse (defToTreeless2 . snd) defs
        ds <- traverse toJava2 ts
        whole <- buildMainMethodMonad ds
        return $ defToText whole

    liftIO $ T.writeFile fileName modText
    -- let defToText = T.pack prettyPrint
    --     fileName = prettyShow (last $ mnameToList modName) ++ ".java"

    -- modText <- runToJavaM opts $ do
    --     ts <- catMaybes <$> traverse (defToTreeless2 . snd) defs
    --     ds <- traverse toJava2 ts
    --     buildMainMethodMonad ds

    -- liftIO $ T.writeFile fileName modText