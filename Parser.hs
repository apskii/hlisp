module Lisp.Parser ( parse ) where

import Lisp.T

import Text.Parsec hiding ( parse )
import Text.Parsec.String

biTrim = between spaces spaces

pA <!> pB = (pA >> return ())
        <|> (pB >> return ())

strLit = do s <- between (char '"') (char '"')
                         (many $ noneOf "\"")
            return (StrT s)

chrLit = do string "#\\"
            c <- anyChar            
            return (ChrT c)
            
intLit = do sign   <- option ' ' (char '-')
            digits <- many1 digit
            lookAhead (space <!> oneOf "()" <!> eof)
            return $ IntT $ read (sign : digits)

symbol = do s <- many1 (noneOf "() \t")
            return (SymT s)

atom = biTrim (strLit <|> chrLit <|> try intLit <|> symbol)

expr = biTrim
     $ do char '('
          xs <- do qs <- char '\'' `sepEndBy` spaces
                   x  <- atom <|> expr
                   return $ foldr (\q a -> LisT [q,a]) x
                          $ replicate (length qs) (SymT "quote")
                `sepBy` spaces
          char ')'
          return (LisT xs)

parse :: String -> [T]
parse = either (error . show) id
      . runP (many $ expr <|> atom) () ""

{- I've decided to throw dotted pairs away!
     They are really ugly shit for save-one-cons bytefuckers.

  where    
    pcons (SymT ".") (ConT x (SymT "nil")) = x
    pcons (SymT ".") _                     = error "Illegal place for dot!"
    pcons x          y                     = ConT x y    
    nil                                    = SymT "nil"
-}