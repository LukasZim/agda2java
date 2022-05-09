{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StandaloneDeriving , MultiParamTypeClasses  , FlexibleInstances ,  TypeFamilies , DeriveAnyClass , DefaultSignatures , DeriveGeneric , OverloadedStrings  #-}

module Agda.Compiler.Options where


















import Agda.Compiler.Backend ( EvaluationStrategy(..) )
    

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
-- import Agda.Compiler.Backend (EvaluationStrategy)

deriving instance Generic EvaluationStrategy
deriving instance NFData  EvaluationStrategy

data JavaOptions =
    JavaOptions {
      -- | 
      javaEvaluation :: EvaluationStrategy
    }
    deriving (Generic, NFData)

