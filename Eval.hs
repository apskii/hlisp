{-# LANGUAGE ViewPatterns, DeriveDataTypeable, FlexibleInstances,
             TemplateHaskell, QuasiQuotes #-}
module HLisp.Eval where

import HLisp.T
import HLisp.Id
import HLisp.Env
import HLisp.Parser

import Prelude hiding ( lookup )

import Data.Maybe
import Data.Typeable

import Control.Arrow
-- import Control.Exception



-- Startup environment
nilEnv :: Env T
nilEnv = fromList $ map selfE ["\\","+","-","*","/",":"]
  where selfE x = (x, SymT x)

-- Transformers stack
type Eval = ErrorT LispErr (ReaderT (Env T) Identity)
runEval = runIdentity . runReaderT nilEnv . runErrorT

-- Evaluator
eval :: T -> Eval T

eval (LisT (SymT "quote" : xs))
  | null xs || not (null (tail xs)) = throwError (ArgcErr "quote" Nothing (Just 1))
  | otherwise                       = return (head xs)

eval (LisT (SymT "λ" : xs))
  | null xs || null (tail xs) = throwError (ArgcErr "λ" Just)
  | otherwise                 = return $ foldr mkLam (last xs) (init xs)
  where
    mkLam (SymT s) t = LamT s t
    mkLam _        _ = throw (FormEx "Lambda arg must be symbol!")

eval e (LisT (SymT "?" : xs)) = case xs of
  [c,t,f] -> do c' <- eval e c
                eval e $ case (c' == LisT []) of
                  True  -> f
                  False -> t
  [c,t]   -> eval e $ LisT [SymT "?", c, t, LisT []]
  _       -> throw (wrongArgsCount "?")
  
-- eval e (LisT (SymT "&" : xs)) = case xs of
--  map (eval e) xs
  
eval e (LisT (x : xs)) = do
  f    <- eval e x
  args <- mapM (eval e) xs
  apply e f args

eval e (SymT s) = return $ maybe (LisT []) id (lookup s e)

eval _ x = return x

apply _ (SymT ":") [] = return $ LisT []
apply _ (SymT ":") xs = case (last xs) of 
  LisT ys -> return $ LisT $ foldr (:) ys (init xs)
  _       -> throw (wrongArgType "(:)")

apply e (LamT s b) (x:xs) = eval (insert s x e)
                          $ case xs of [] -> b
                                       ys -> LisT (b : ys)

apply _ o _ = throw (o `isNotA` "function")

run code = print =<< (eval nilEnv $ head $ parse code)

t1 = run "(: 1 2 3 4 5 6 7 8 9 ())"
-- > LisT [IntT 1,IntT 2,IntT 3,IntT 4,IntT 5,IntT 6,IntT 7,IntT 8,IntT 9]

t2 = run "(: (λ x x) 7 '(8 9 10))"
-- > LisT [LamT "x" (SymT "x"),IntT 7,IntT 8,IntT 9,IntT 10]

t3 = run "((λ λ (λ λ)) λ)"

-- eval (ConT (SymT f@"car") t) = case t of

-- eval (eVw -> Fcall f@"car")

-- eval (ConT (SymT "and") xs) =
-- eval x = x

{-
nilEnv = HT.fromList $ map (id *** parse)
           [ ("t", ) ]
-}

-- LispM

{-
(set 'inc (\ x (+ 1 x)))
-}

{- Evaluation view for T:
--   for handy pattern-matching on call-forms

data EvaluationView = Fcall String T
                    | Other

eVw (ConT (SymT s) t) = Fcall s t
eVw _                 = Other
-}

  -- ConT _ (SymT   _  ) -> throw (ArgEx "Quote cannot be called on dotted pairs!")

{-
-- View on list from back
data BackView a = [a] :| a
                | Empty

backV xs | null xs || null (tail xs) = init xs :| last xs
         | otherwise                 = Empty



(def m
  (@ a b c d
     (+ a b c d)))

(def some-function
  (\ a b c d
     (+ a b c d)))

(: 1 ())

-}