{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, LambdaCase, UnicodeSyntax #-}
module HLisp.Eval ( evalT, EvalT, runEvalT ) where

import HLisp.Types

import Prelude hiding ( lookup, length, foldr, foldr1, error )
import Prelude.Unicode

import Data.Data hiding ( typeOf )
import Data.List ( genericLength )
import Data.Foldable

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Arrow

-- Utilities
len   = genericLength
error = throwError

-- Evaluation transformers stack
type EvalT = ErrorT LispErr (StateT Env IO)
runEvalT = flip evalStateT nilEnv ∘ runErrorT

-- Term evaluator
evalT ∷ T → EvalT T

evalT (LisT (SymT s : xs)) | s ∈ ["λ", "quote", "?" , "def"] = evalSF s xs

evalT (SymT s) = (lookup s <$> get) >>= \case
  Just t  → return t
  Nothing → error $ UnboundSymErr s

evalT (LisT (x : xs)) = join $ apply <$> evalT x <*> mapM evalT xs

evalT x = return x

-- Special form evaluator
evalSF "λ" xs | len xs < 2 = return $ symApp "λ" xs

evalSF "λ" (SymT s : xs) = return $ AbsT s (foldr1 mkLam xs)
  where
    mkLam s b = LisT [SymT "λ", s, b]

evalSF "λ" (t : _) = error $ ArgTyErr "λ" symTy t

evalSF "quote" [x] = return x
evalSF "quote" xs  = error $ ArgcErr "quote" (len xs) (ArgcE 1)

evalSF "?" [c,t,f] = evalT c >>= \case
  LisT []   → evalT f
  otherwise → evalT t
  
evalSF "?" [c,t] = evalSF "?" [c, t, LisT []]
evalSF "?" args  = error $ ArgcErr "?" (len args) (ArgcR 2 3)

evalSF "def" [SymT name, t] = SymT name <$ (modify ∘ extend name =<< evalT t)
evalSF "def" [t,         _] = error $ ArgTyErr "def" symTy t
evalSF "def" args           = error $ ArgcErr "def" (len args) (ArgcE 2)

-- Applicator
apply (LisT (x : xs)) ys = apply x (xs ++ ys)

apply (SymT s) xs
  | len xs < arity = return $ symApp s xs
  | otherwise      = f xs
  where
    (arity, f) = case (lookup s builtIns) of
      Just b  → b
      Nothing → (0, \_ → error $ UnboundSymErr s)

apply (AbsT v t) (x : xs) = do
  r ← evalT (subst v (symApp "quote" [x]) t)
  case xs of
    [] → return r
    xs → apply r xs

apply t _ = error $ NonApplicableErr t

-- Built-In's
builtIns = fromList [
  -- control
  "λ" # 2 # bLam,
  -- arith
  "+" # 2 # bAdd,
  "-" # 2 # bSub,
  "*" # 2 # bMul,
  "/" # 2 # bDiv,
  -- equality
  "=" # 2 # bEq,
  "<" # 2 # bLT,
  -- lists
  ":"  # 2 # bCons,
  "hd" # 1 # bHd,
  "tl" # 1 # bTl,
  -- reflection
  "type-of" # 1 # bTypeOf
 ]
  where
    infixr #
    (#) = (,)
           
-- Startup environment
nilEnv ∷ Env
nilEnv = fromList $ map (id &&& SymT) ("T" : syms builtIns)

(#) = (#)

intTy = IntT (#)
lisTy = LisT (#)
symTy = SymT (#)

infErr fn ts@(len → tc) xs@(len → argc)
  | tc ≢ argc = error $ ArgcErr fn argc (ArgcE tc)
  | otherwise = case find snd $ zipWith (\t a → ((t,a), a `isnt` t)) ts xs of
    Just ((t,a), _) → error $ ArgTyErr fn t a
    Nothing         → error $ OtherErr "Incorrect error! :)"
  where
    isnt obj ty = toConstr obj ≢ toConstr ty

bLam xs = evalT $ LisT (SymT "λ" : xs)

bAdd [IntT x, IntT y] = return $ IntT (x + y)
bAdd xs               = infErr "+" [intTy, intTy] xs

bSub [IntT x, IntT y] = return $ IntT (x - y)
bSub xs               = infErr "-" [intTy, intTy] xs

bMul [IntT x, IntT y] = return $ IntT (x * y)
bMul xs               = infErr "*" [intTy, intTy] xs

bDiv [IntT x, IntT y] = return $ IntT (x `div` y)
bDiv xs               = infErr "/" [intTy, intTy] xs

bCons [hd, LisT tl]    = return $ LisT (hd : tl)
bCons [_,  t      ]    = error $ ArgTyErr ":" lisTy t
bCons xs               = error $ ArgcErr ":" (len xs) (ArgcE 2)

bEq [a, b] | a ≡ b     = return $ SymT "T"
           | otherwise = return $ LisT []

bLT [IntT a, IntT b]
  | a < b     = return $ SymT "T"
  | otherwise = return $ LisT []
bLT xs        = infErr "<" [intTy, intTy] xs

bHd [LisT []   ] = error $ OtherErr "Empty list!"
bHd [LisT (x:_)] = return x
bHd xs           = infErr "hd" [lisTy] xs

bTl [LisT []    ] = error $ OtherErr "Empty list!"
bTl [LisT (_:xs)] = return $ LisT xs
bTl xs            = infErr "tl" [lisTy] xs

bTypeOf [t] = return $ SymT (typeOf t)
bTypeOf xs  = error $ ArgcErr "type-of" (len xs) (ArgcE 1)