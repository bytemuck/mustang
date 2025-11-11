module Lex
  ( expression,
  )
where

import Control.Applicative (empty, many, some, (<|>))
import Mustang.Parser (ParserM)
import Mustang.Parser.Primitive (noneOf, token)
import Mustang.Parser.Text (digit, spaces)
import SExp (SAtom (Identifier, Number, String), SExp (..))

expression :: ParserM Char SExp
expression = spaces *> (atom <|> list) <* spaces

atom :: ParserM Char SExp
atom = pString <|> number <|> identifier

pString :: ParserM Char SExp
pString = do
  tok <- token '"' *> many (noneOf "\"") <* token '"'
  return $ Atom $ String tok

number :: ParserM Char SExp
number = do
  tok <- some digit
  case reads tok :: [(Integer, String)] of
    [(n, "")] -> return $ Atom (Number n)
    _ -> empty

identifier :: ParserM Char SExp
identifier = do
  h <- noneOf " ()\n\t\r"
  t <- many (noneOf " ()\n\t\r")
  return $ Atom $ Identifier (h : t)

list :: ParserM Char SExp
list = do
  exprs <- token '(' *> spaces *> some expression <* spaces <* token ')'
  return $ List exprs