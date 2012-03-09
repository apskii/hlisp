{-# LANGUAGE GADTs, QuasiQuotes #-}
module HLisp.Types ( T(..), Id, Env, lookup, extend, empty ) where

import HLisp.Pretty

import qualified Data.Map as M
import Prelude hiding ( lookup )

import GHC.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec
import Text.InterpolatedString.Perl6

import Control.Monad.Error

-- Basic lisp top-type
data T = SymT Id
       | StrT String
       | IntT Integer
       | ChrT Char
       | LisT [T]
       | CloT Env Id T
       deriving (Eq, Show, Read)

-- Identifiers
type Id = String

-- Environment
type Env = M.Map Id T

lookup :: Id -> Env -> Maybe T
lookup = M.lookup

extend :: Id -> T -> Env -> Env
extend = M.insert

empty = M.empty

-- Errors
data LispErr = ArgcErr  Id Integer String
             | ArgTyErr Id Id T
             deriving (Eq, Show, Read)

wrongArgc  = "Wrong args count in call of"
wrongArgTy = "Wrong arg type in call of"

instance Pretty LispErr where  
  
  pretty (ArgcErr fn gN eS) =
    [qc| $wrongArgc `$fn': $gN given, while expecting $eS. |]
  
  pretty (ArgTyErr fn e t) =
    [qc| $wrongArgTy `$fn': expected `$e' in
           $(pretty t) |]


-- Builtins
data BuiltIn where
  BuiltIn :: (MonadError LispErr m) 
          => Id -> (T -> m T) 
          -> BuiltIn

instance Eq BuiltIn where
  (BuiltIn fnA _) == (BuiltIn fnB _) = fnA == fnB

instance Show BuiltIn where
  show (BuiltIn fn _) = "BuiltIn " ++ fn

instance Read BuiltIn where
  readPrec = prec 10 
           $ do Ident "BuiltIn" <- lexP
                fn <- step readPrec
                return $ BuiltIn fn (getBuiltIn fn)

lamBuiltIn f = BuiltIn "λ-built-in" f
                
getBuiltIn fn = case fn of
  "type-of" -> bTypeOf
  "+"       -> bSum

bTypeOf = return . typeOf

bSum (IntT x) = return (BuiltIn plus)
  where plus (IntT y) = return $ IntT (x + y)
        plus t        = throwError $ ArgTyErr "+" 2 "integer" t
        
bSum t = throwError $ ArgTyErr "+" 1 "" t


{-
data T where 
  SymT :: Id          -> T
  StrT :: String      -> T
  IntT :: Integer     -> T
  ChrT :: Char        -> T
  LisT :: [T]         -> T
  CloT :: Env         -> T
  FunT :: BuiltIn -> T-}

-- deriving instance Show T
-- deriving instance Read T
-- deriving instance   Eq T