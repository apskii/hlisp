{-# LANGUAGE ViewPatterns, DeriveDataTypeable, FlexibleInstances,
             TemplateHaskell, QuasiQuotes #-}
module HLisp.Eval ( eval, evalString ) where

import HLisp.Types
import HLisp.Parser
import HLisp.Pretty

import Prelude hiding ( lookup, length, foldr, foldr1 )

import System.IO

import Data.Data hiding ( typeOf )
import Data.List ( genericLength )
import Data.Maybe
import Data.Function
import Data.Foldable

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Control.Applicative

import Text.InterpolatedString.Perl6

length = genericLength
m ?: x = maybe x id m

local env m = do
  oldEnv <- get 
  put env
  x <- m
  put oldEnv
  return x

-- Transformers stack
type Eval = ErrorT LispErr (StateT Env Identity)
runEval = runIdentity . flip evalStateT nilEnv . runErrorT

-- Evaluator
eval0 :: T -> Eval T

eval0 (LisT (SymT "quote" : xs)) = case xs of
  [x] -> return x
  _   -> throwError $ ArgcErr "quote" (length xs) (ArgcE 1)

eval0 t@(LisT (SymT "λ" : xs))
  | length xs < 2 = return t
  | otherwise     = case xs of
    SymT s : ys -> get >>= \e -> return $ CloT e s (foldr1 mkLam ys)
    t      : _  -> throwError $ ArgTyErr "λ" symTy t
  where
    mkLam s b = LisT [SymT "λ", s, b]

eval0 (LisT (SymT "?" : xs)) = case xs of
  [c,t,f] -> do
    c' <- eval0 c
    eval0 $ case c' of
      LisT []   -> f
      otherwise -> t

  [c,t] -> eval0 $ LisT [SymT "?", c, t, LisT []]
  _     -> throwError $ ArgcErr "?" (length xs) (ArgcR 2 3)

eval0 (LisT (SymT "def" : xs)) = case xs of
  [SymT name, t] -> do 
    x <- eval0 t
    let y = case x of
              CloT e s b -> CloT (extend name y e) s b
              _          -> x
    modify (extend name y)
    return (SymT name)                         
    
  [t, _] -> throwError $ ArgTyErr "def" symTy t
  _      -> throwError $ ArgcErr "def" (length xs) (ArgcE 2)

eval0 (SymT s) = do
  e <- get
  case lookup s e of
    Just t  -> return t
    Nothing -> throwError $ UnboundSymErr s

eval0 (LisT (x : xs)) = do
  f    <- eval0 x
  args <- mapM eval0 xs
  apply f args

eval0 x = return x

apply (LisT (x : xs)) ys = apply x (xs ++ ys)

{-
apply (SymT s) xs@(length -> argc) =
  case lookup s builtIns of 
    Just (arity, f) | argc < arity -> return $ LisT (SymT s : xs)
                    | otherwise    -> f xs
    Nothing -> do
      e <- get
      case lookup s e of
        Just y  -> apply y xs
        Nothing -> throwError (UnboundSymErr s)
-}          

apply (SymT s) xs@(length -> argc)
  | argc < arity = return $ LisT (SymT s : xs)
  | otherwise = f xs
  where
    (arity, f) = lookup s builtIns
                 ?: (0, \_ -> throwError (UnboundSymErr s))

apply (CloT e s b) (x : xs)
  = local (extend s x e)
  $ eval0 b >>= case xs 
                  of [] -> return
                     ys -> flip apply xs

apply t _ = throwError (NonApplicableErr t)

eval = runEval . eval0

-- Built-In's
builtIns = fromList [ ("λ", (2, bLam))
                    , ("+", (2, bAdd))
                    , ("-", (2, bSub)) 
                    , ("*", (2, bMul))
                    , ("/", (2, bDiv)) 
                    , ("=", (2, bEq))
                    , ("<", (2, bLT)) 
                    , (":", (2, bCons))
                    
                    , ("hd", (1, bHd))
                    , ("tl", (1, bTl))
                    
                    , ("type-of", (1, bTypeOf))
                    ]
           
-- Startup environment
nilEnv :: Env
nilEnv = fromList $ map selfEvaluated ("T" : syms builtIns)
  where selfEvaluated x = (x, SymT x)

(#) = undefined

intTy = IntT (#)
lisTy = LisT (#)
symTy = SymT (#)

infErr fn ts@(length -> tc) xs@(length -> argc)
  | tc /= argc = throwError $ ArgcErr fn argc (ArgcE tc)
  | otherwise  = case find snd $ zipWith (\t a -> ((t,a), a `isnt` t)) ts xs of
    Just ((t,a), _) -> throwError $ ArgTyErr fn t a
    Nothing         -> throwError $ OtherErr "Incorrect error! :)"
  where
    isnt obj ty = toConstr obj /= toConstr ty

bLam xs = eval0 $ LisT (SymT "λ" : xs) -- kinda black sorcery

bAdd [IntT x, IntT y] = return $ IntT (x + y)
bAdd xs               = infErr "+" [intTy, intTy] xs

bSub [IntT x, IntT y] = return $ IntT (x - y)
bSub xs               = infErr "-" [intTy, intTy] xs

bMul [IntT x, IntT y] = return $ IntT (x * y)
bMul xs               = infErr "*" [intTy, intTy] xs

bDiv [IntT x, IntT y] = return $ IntT (x `div` y)
bDiv xs               = infErr "/" [intTy, intTy] xs

bCons [hd, LisT tl]    = return $ LisT (hd : tl)
bCons [_,  t      ]    = throwError $ ArgTyErr ":" lisTy t
bCons (length -> argc) = throwError $ ArgcErr ":" argc (ArgcE 2)

bEq [a, b] | a == b    = return $ SymT "T"
           | otherwise = return $ LisT []

bLT [IntT a, IntT b]
  | a < b     = return $ SymT "T"
  | otherwise = return $ LisT []
bLT xs        = infErr "<" [intTy, intTy] xs

bHd [LisT []   ] = throwError $ OtherErr "Empty list!"
bHd [LisT (x:_)] = return x
bHd xs           = infErr "hd" [lisTy] xs

bTl [LisT []    ] = throwError $ OtherErr "Empty list!"
bTl [LisT (_:xs)] = return $ LisT xs
bTl xs            = infErr "tl" [lisTy] xs

bTypeOf [t]              = return $ SymT (typeOf t)
bTypeOf (length -> argc) = throwError $ ArgcErr "type-of" argc (ArgcE 1)


evalString = either (error . pretty) (pretty . last)
           . runEval . sequence . map eval0 . parse form


test = do hSetEncoding stdout utf8
          putStrLn $ evalString [q|
                               
  (def id
    (λ x x))
                                 
  (def map 
    (λ f xs
      (? xs (: (f (hd xs))
               (map f (tl xs))))))

  (def <=
    (λ a b
      (< a (+ 1 b))))
  
  (def ..
    (λ s e
      (? (<= s e)
         (: s (.. (+ 1 s) e)))))

  (def @
    (λ f x y
      (f y x)))

  (def .
    (λ f g x
      (f (g x))))

  (def .:
    (λ f g x y
      (f (g x y))))

  (def not (= ()))

  (def /= (.: not =))

  (def is
    (λ t
      (. (= t)
         type-of)))

  (def isnt
    (λ t
      (. (/= t)
         type-of)))

  (def Atom?     (isnt 'List))
  (def Function? (is 'Function))

  (def foldl
    (λ f s xs
      (? xs (foldl f
              (f s (hd xs)) 
              (tl xs))
            s)))

  (def foldl1
    (λ f xs
      (? xs (foldl f
              (hd xs)
              (tl xs)))))

  (def reverse
    (λ xs (foldl (@ :) () xs)))

  (def [ '[)
  (def ] '])

  (def <~
    (λ f m x
      ((λ y
         (? (Function? y)
            (<~ f y)
            (f y)))
       (m x))))

  (def vararg-collector/r
    (λ acc x
      (? (= x ])
         (reverse acc)
         (vararg-collector/r (: x acc)))))

  (def vararg-collector/l
    (λ acc n x
      (? (= x [)
         (vararg-collector/r ())
         (? (< n 2)
            (reverse (: x acc))
            (vararg-collector/l
              (: x acc)
              (- n 1))))))

  (def vararg-builder
    (λ f n
      (<~ f (vararg-collector/l () n))))

  (def ~+ +)

  (def + (vararg-builder (foldl1 ~+) 2))

  (def t1 (+ [ 1 2 3))
  (def t2 (t1 3 4 5 6))
  (def t3 (t2 7 8 9 ]))

  (+ 2 t3)

|]