{-# LANGUAGE QuasiQuotes #-}
module HLisp.Error ( LispErr(..) ) where

import HLisp.T
import HLisp.Id
import HLisp.Pretty

import Text.InterpolatedString.Perl6

data LispErr = ArgcErr  Id (Maybe Integer) (Maybe Integer)
             | ArgTyErr Id Id T
             deriving (Show,Read)

wrongArgc  = "Wrong args count in call of"
wrongArgTy = "Wrong arg type in call of"

instance Pretty LispErr where  
  
  pretty (ArgcErr fn (Just gN) (Just eN)) =
    [qc| $wrongArgc $fn: $gN given, while expecting $eN. |]

  pretty (ArgcErr fn (Just gN) Nothing) =
    [qc| $wrongArgc $fn: $gN given. |]

  pretty (ArgcErr fn Nothing (Just eN)) =
    [qc| $wrongArgc $fn: expecting $eN. |]

  pretty (ArgcErr fn Nothing Nothing) =
    [qc| $wrongArgc $fn. |]
  
  pretty (ArgTyErr fn e t) =
    [qc| $wrongArgTy $fn: expected `$e` in
           $(pretty t) |]