module HLisp.T ( T(..) ) where

data T = SymT String
       | StrT String
       | ChrT Char
       | IntT Integer
       | LisT [T]
       | LamT String T
       deriving (Eq,Show,Read)