module HLisp.BuiltIns where

import HLisp.Types

-- +
-- type-of

type BuiltIn = T -> m T

typeOf t = case t of
  (SymT _)     -> "symbol"
  (StrT _)     -> "string"  
  (IntT _)     -> "integer"
  (ChrT _)     -> "character"
  (LisT _)     -> "list"
  (CloT _ _ _) -> "function"

-- lamBuiltIn f = BuiltIn "λ-built-in" f

getBuiltIn fn = case fn of
  "type-of" -> bTypeOf
  "+"       -> bSum

bTypeOf = return . typeOf

bSum (IntT x) = return (lamBuiltIn plus)
  where plus (IntT y) = return $ IntT (x + y)
        plus t        = throwError $ ArgTyErr "+" 2 "integer" t
        
bSum t = throwError $ ArgTyErr "+" 1 "" t
