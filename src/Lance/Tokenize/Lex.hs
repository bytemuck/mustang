module Lance.Tokenize.Lex
  ( expression,
  )
where

import Control.Applicative (empty, many, some, (<|>))
import Lance.Tokenize.Primitive (noneOf, token)
import Lance.Tokenize.Text (digit, spaces)
import Lance.Tokenize.Tokenize (ParserM)
import Lance.Tokenize.TokenizedExpr (LocatedTExpr (LocatedTExpr), TAtom (Identifier, Number, String), TExpr (..), getPosition)

expression :: ParserM Char LocatedTExpr
expression = spaces *> (atom <|> list) <* spaces

atom :: ParserM Char LocatedTExpr
atom = do
  position <- getPosition
  LocatedTExpr position <$> (pString <|> number <|> identifier)

pString :: ParserM Char TExpr
pString = do
  tok <- token '"' *> many (noneOf "\"") <* token '"'
  return $ Atom $ String tok

number :: ParserM Char TExpr
number = do
  tok <- some digit
  case reads tok :: [(Integer, String)] of
    [(n, "")] -> return $ Atom (Number n)
    _ -> empty

identifier :: ParserM Char TExpr
identifier = do
  h <- noneOf " ()\n\t\r"
  t <- many (noneOf " ()\n\t\r")
  return $ Atom $ Identifier (h : t)

list :: ParserM Char LocatedTExpr
list = do
  position <- getPosition
  exprs <- token '(' *> many expression <* token ')'
  return $ LocatedTExpr position (List exprs)