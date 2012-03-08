module HLisp.Main where

import HLisp.T
import HLisp.Parser

-- import Lisp.Pretty

main = parse stdParser
     $ "(define (1+ x)\
       \  (+ 1 x))"