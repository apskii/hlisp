module Lisp.Main where

import Lisp.T
import Lisp.Parser

-- import Lisp.Pretty

f x  = case () of
  () | x > 2 -> "1"
     | x > 9 -> "9"
                


{-
    ...
    pwd <- getLine `retryUntil` isValid
    ...
-}

{-
main = parse
     $ "(define (1+ x)\
       \  (+ 1 x))"
-}