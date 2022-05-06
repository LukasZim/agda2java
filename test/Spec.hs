
import Lib ()
import Agda.Compiler.ToJava ()
import Agda.Compiler.Compiler ( javaBackend )
import Agda.Compiler.Backend ()
import Agda.Main ( runAgda )

main :: IO ()
-- main = someFunc
main = runAgda [javaBackend]
