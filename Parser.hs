module HLisp.Parser ( parse ) where

import HLisp.T

import Data.Function.Pointless
import Control.Applicative hiding ( (<|>), many )

import Text.Parsec hiding ( parse )
import Text.Parsec.String

(~|~)  = (() <$) .: (<|>)

strLit = StrT <$> (char '"' *> many (noneOf "\"") <* char '"')

chrLit = ChrT <$> (string "#\\" *> anyChar)

intLit = IntT . read .: (:) <$> option ' ' (char '-') <*> many1 digit
                            <*  lookAhead (space ~|~ oneOf "()" ~|~ eof)

symbol = SymT <$> many1 (noneOf "() \t")

atom   = strLit <|> chrLit <|> try intLit <|> symbol

expr   = LisT <$> (char '(' *> (form `sepBy` spaces) <* char ')')

quoted = LisT . (SymT "quote" :) . (:[]) <$> (char '\'' *> spaces *> form)

form   = quoted <|> expr <|> atom

parse  = either (error . show) id . runP top () ""
  where top = many (spaces *> form <* spaces)

parse :: String -> [T]
