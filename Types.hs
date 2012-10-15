{-# LANGUAGE QuasiQuotes, NoMonomorphismRestriction, DeriveDataTypeable, LambdaCase, MultiWayIf, UnicodeSyntax #-}
module HLisp.Types
       ( T(..), symApp, subst, typeOf, Id
       , Env, lookup, extend, empty, fromList, syms
       , ArgcQ(..), LispErr(..)
       , Pretty(..)
       ) where

import Prelude hiding ( lookup )
import Prelude.Unicode
import Text.InterpolatedString.Perl6

import qualified Data.Map as M
import Data.List hiding ( lookup )
import Data.Data hiding ( typeOf )

import Control.Monad.Error

-- Basic lisp top-type
data T = SymT Id
       | StrT String
       | IntT Integer
       | ChrT Char
       | LisT [T]
       | AbsT Id T
       deriving (Eq, Show, Read, Typeable, Data)

class Pretty a where
  pretty ∷ a → String

instance Pretty T where
  pretty = \case
    SymT name  → name
    StrT str   → "\"" ++ str ++ "\""
    IntT int   → show int
    ChrT chr   → show chr
    LisT xs    → "(" ++ intercalate " " (map pretty xs) ++ ")"
    AbsT var t → [qq|(λ $var {pretty t})|]

symApp sym args = LisT (SymT sym : args)

subst var val term = transform term
  where        
    transform = \case
      
      sym@(SymT name) → if
        | name ≡ var → val
        | otherwise  → sym

      abs@(AbsT var' term') → if
        | var ≡ var' → abs
        | otherwise  → AbsT var' (transform term')

      lam@(LisT (SymT "λ" : xs)) → if
        | SymT var ∈ init xs → lam
        | otherwise          → symApp "λ" (init xs ++ [transform (last xs)])

      q@(LisT (SymT "quote" : _)) → q

      LisT xs → LisT (map transform xs)
      other   → other

typeOf = \case
  SymT _   → "Symbol"
  StrT _   → "String"
  IntT _   → "Integer"
  ChrT _   → "Char"
  LisT _   → "List"
  AbsT _ _ → "Function"

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
  pretty = \case
    ArgcE  n   → [qq|exactly $n|]
    ArgcGE n   → [qq|at least $n|]
    ArgcR  s e → [qq|from $s to $e|]

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
  pretty = \case
    
    ArgcErr fn g e → [qq|
      $wrongArgc `{fn}': $g given, {pretty e} expected. |]
  
    ArgTyErr fn e t → [qq|
      $wrongArgTy `{fn}':
        `{typeOf t}' given, `{typeOf e}' expected
      in
        {pretty t} |]

    UnboundSymErr s → [qq|
      Symbol `{s}' is unbound! |]

    NonApplicableErr t → [qq|
      Tried to apply non-applicable value:
        {pretty t} |]

    OtherErr s → [qq|
      $s |]