module Agda.Compiler.Compiler where
import Agda.Compiler.Backend
    ( EvaluationStrategy(EagerEvaluation),
      Backend(..),
      Backend'(..),
      Recompile(Recompile),
      IsMain,
      Flag,
      ModuleName(mnameToList),
      Definition,
      TCM )
import Agda.Compiler.Options ( JavaOptions(JavaOptions) )
import Agda.Compiler.Syntax (JavaEntry, JavaModule)
import Agda.Interaction.Options (OptDescr)
import Agda.Compiler.Common (compileDir)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Java.Pretty ( prettyPrint )
-- import Data.Map
import Data.Text (Text, pack, unpack)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Agda.Utils.Pretty (prettyShow)
import Agda.Compiler.ToJava
    ( ToJava(toJava), buildMainMethodMonad, runToJavaM, defToTreeless, buildRunFunction, JavaStmt, getInside, getOutside, ToJavaState (toJavaFuns), getJavaFunctionNames )
import Language.Java.Syntax (CompilationUnit, Block (Block), Modifier (Public))
import Data.Maybe ( catMaybes )
import Agda.Compiler.ToJava


test :: String
test = "testCase"

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

javaPostModule :: JavaOptions -> () -> IsMain -> ModuleName -> [(IsMain, Definition)] -> TCM ()
javaPostModule opts _ ismain modName defs = do
    let defToText :: CompilationUnit -> Text
        defToText = T.pack . prettyPrint
        fileName = prettyShow (last $ mnameToList modName) ++ ".java"
        preamble = pack "\
        \interface Visitor {}\n\n\
        \interface Agda {}\n\n\
        \interface AgdaData extends Agda {\n\
        \    Agda match(Visitor visitor);\n\
        \}\n\n\
        \interface AgdaFunction extends Agda{\n\
        \    Visitor getFn();\n\
        \}\n\n\
        \interface AgdaLambda extends Agda {\n\
        \    Agda run(Agda arg);\n\
        \}\n\n\
        \class AgdaFunctionImpl implements AgdaFunction {\n\
        \    private Visitor fn;\n\
        \    public AgdaFunctionImpl(Visitor fn) {\n\
        \        this.fn = fn;\n\
        \    }\n\
        \    public Visitor getFn() {\n\
        \        return fn;\n\
        \    }\n\
        \}\n\n\
        \"
    
        
    modText <- runToJavaM opts $ do
        ts <- catMaybes <$> traverse (defToTreeless . snd) defs
        ds <- traverse toJava ts
        let d = buildRunFunction
            dds = concat ds
            insideMain = concat $catMaybes $ Prelude.map getInside dds
            -- test = toList gets toJavaFuns
            outsideMain = catMaybes $ Prelude.map getOutside dds
        moreOutside <- getJavaFunctionNames
        let fields = createFields moreOutside
            moreOutsideMain = fields ++  outsideMain
        whole <- buildMainMethodMonad insideMain $ d : moreOutsideMain
        return $ defToText whole

    liftIO $ T.writeFile fileName (preamble <> modText)


