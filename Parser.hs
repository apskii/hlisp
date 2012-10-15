{-# LANGUAGE UnicodeSyntax #-}
module HLisp.Parser ( parse ) where

import HLisp.Types

import Text.Parsec hiding ( parse )

import Prelude.Unicode
import Control.Applicative hiding ( (<|>), many )

infixl 8 ∘:
(f ∘: g) x y = f (g x y)

(~|~)  = (() <$) ∘: (<|>)

strLit = StrT <$> (char '"' *> many (noneOf "\"") <* char '"')

chrLit = ChrT <$> (string "#\\" *> anyChar)

intLit = IntT ∘ read ∘: (:) <$> option ' ' (char '-') <*> many1 digit
                            <*  lookAhead (space ~|~ oneOf "()" ~|~ eof)

symbol = SymT <$> many1 (noneOf "() \n\t\r")

atom   = strLit <|> chrLit <|> try intLit <|> symbol

expr   = LisT <$> (char '(' *> spaces *> (form `sepEndBy` spaces) <* char ')')

quoted = LisT ∘ (SymT "quote" :) ∘ (:[]) <$> (char '\'' *> spaces *> form)

form   = quoted <|> expr <|> atom

top    = many (spaces *> form <* spaces)

parse ∷ String -> Either ParseError [T]
parse = runParser top () ""
