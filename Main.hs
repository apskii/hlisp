{-# LANGUAGE QuasiQuotes #-}
module HLisp.Main where

import HLisp.Eval
import System.IO
import Text.InterpolatedString.Perl6

main = do hSetEncoding stdout utf8
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

  (def isnt (.: not is))

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

  t3
|]