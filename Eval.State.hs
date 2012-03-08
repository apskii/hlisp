module HLisp.Eval.State ( State(..) ) where

import HLisp.T

import Data.Map

data State = State { tape :: [T]
                   , env  :: Map String T
                   }