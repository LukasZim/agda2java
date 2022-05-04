module Agda.Compiler.Options where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data JavaOptions = 
    JavaOptions {}
    deriving (Generic, NFData)


defaultJavaOptions :: JavaOptions
defaultJavaOptions = JavaOptions {}