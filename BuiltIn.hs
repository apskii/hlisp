{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module HLisp.BuiltIn ( BuiltIn(..) ) where

import HLisp.Id

import GHC.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec

data BuiltIn t = BuiltIn Id (t -> t)

class GetBuiltIn t where
  getBuiltIn :: Id -> BuiltIn t

instance Eq (BuiltIn t) where
  (BuiltIn fnA _) == (BuiltIn fnB _) = fnA == fnB

instance Show (BuiltIn t) where
  show (BuiltIn fn _) = "BuiltIn " ++ fn

instance GetBuiltIn t => Read (BuiltIn t) where
  readPrec = prec 10 
           $ do Ident "BuiltIn" <- lexP
                fn <- step readPrec
                return (getBuiltIn fn)
  