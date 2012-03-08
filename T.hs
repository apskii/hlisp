module HLisp.T ( T(..) ) where

import HLisp.Id
import HLisp.Env
import HLisp.BuiltIn

data T = SymT Id
       | StrT String
       | IntT Integer
       | ChrT Char
       | LisT [T]
       | CloT (Env T) Id T
       | FunT (BuiltIn T)
       deriving (Eq, Show, Read)
