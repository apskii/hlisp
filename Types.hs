{-# LANGUAGE GADTs, QuasiQuotes, NoMonomorphismRestriction, DeriveDataTypeable #-}
module HLisp.Types 
       ( T(..), typeOf, Id
       , Env, lookup, extend, empty, fromList, syms
       , ArgcQ(..), LispErr(..)
       ) where

import HLisp.Pretty

import Prelude hiding ( lookup )

import qualified Data.Map as M
import Data.Data hiding ( typeOf )
import Data.List ( intercalate )

import Text.InterpolatedString.Perl6

import Control.Monad.Error

-- Basic lisp top-type
data T = SymT Id
       | StrT String
       | IntT Integer
       | ChrT Char
       | LisT [T]
       | CloT Env Id T
       deriving (Eq, Show, Read, Typeable, Data)

instance Pretty T where
  pretty (SymT s)     = s
  pretty (StrT s)     = "\"" ++ s ++ "\""
  pretty (IntT i)     = show i
  pretty (ChrT c)     = show c
  pretty (LisT xs)    = "(" ++ intercalate " " (map pretty xs) ++ ")"
  pretty (CloT e s t) = [qq|(#λ $s {pretty t})|]

typeOf t = case t of
  (SymT _)     -> "Symbol"
  (StrT _)     -> "String"
  (IntT _)     -> "Integer"
  (ChrT _)     -> "Char"
  (LisT _)     -> "List"
  (CloT _ _ _) -> "Function"

-- Identifiers
type Id = String

-- Environment
type Env = M.Map Id T

lookup   = M.lookup
extend   = M.insert
empty    = M.empty
fromList = M.fromList
syms     = M.keys

-- Type for specifying relative args count
data ArgcQ = ArgcE  Integer
           | ArgcGE Integer
           | ArgcR  Integer Integer
           deriving (Eq, Show, Read)
             
instance Pretty ArgcQ where
  pretty (ArgcE  n)  = [qq|exactly $n|]
  pretty (ArgcGE n)  = [qq|at least $n|]
  pretty (ArgcR s e) = [qq|from $s to $e|]

-- Errors
data LispErr = ArgcErr Id Integer ArgcQ
             | ArgTyErr Id T T
             | UnboundSymErr Id
             | NonApplicableErr T
             | OtherErr String
             deriving (Eq, Show, Read)

instance Error LispErr where
  strMsg = OtherErr

wrongArgc  = "Wrong args count in call of"
wrongArgTy = "Wrong arg type in call of"

instance Pretty LispErr where  
  
  pretty (ArgcErr fn g e) = [qq|
    $wrongArgc `{fn}': $g given, {pretty e} expected. |]
  
  pretty (ArgTyErr fn e t) = [qq|
    $wrongArgTy `{fn}':
      `{typeOf t}' given, `{typeOf e}' expected
    in
      {pretty t} |]

  pretty (UnboundSymErr s) = [qq|
    Symbol `{s}' is unbound! |]

  pretty (NonApplicableErr t) = [qq|
    Tried to apply non-applicable value:
      {pretty t} |]

  pretty (OtherErr s) = [qq|
    $s |]