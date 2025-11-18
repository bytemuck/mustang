module Lex
  ( expression,
  )
where

import Control.Applicative (empty, many, some, (<|>))
import Mustang.Parser (ParserM)
import Mustang.Parser.Primitive (noneOf, token)
import Mustang.Parser.Text (digit, spaces)
import SExp (LocatedSExp (LocatedSExp), SAtom (Identifier, Number, String), SExp (..), getPosition)

expression :: ParserM Char LocatedSExp
expression = spaces *> (atom <|> list) <* spaces

atom :: ParserM Char LocatedSExp
atom = do
  position <- getPosition
  LocatedSExp position <$> (pString <|> number <|> identifier)

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

list :: ParserM Char LocatedSExp
list = do
  position <- getPosition
  exprs <- token '(' *> many expression <* token ')'
  return $ LocatedSExp position (List exprs)