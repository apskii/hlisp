module HLisp.Pretty ( pretty ) where

import HLisp.T

{-
pretty t = case t of
  SymT s   -> s
  StrT s   -> "\"" ++ s ++ "\""
  ChrT c   -> '\'' : c : "'"
  IntT i   -> show i
  ConT l r -> "(" ++ pretty l ++ prettyR r ++ ")"
    where
      prettyR c@(ConT _ _) = " " ++ (init $ tail $ pretty c)
      prettyR (SymT "nil") = ""
      prettyR r            = " . " ++ pretty r
-}