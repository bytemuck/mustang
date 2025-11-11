module Mustang.Parser.Primitive
  ( satisfy,
    anyToken,
    token,
    oneOf,
    noneOf,
    between,
    surround,
    chainl1,
    chainl,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except
import Control.Monad.State (get, put)
import Mustang.Parser (ParseError (Empty), ParserM (ParserM))

satisfy :: (s -> Bool) -> ParserM s s
satisfy p = ParserM $ do
  input <- get
  case input of
    [] -> throwError Empty
    (x : xs) ->
      if p x
        then
          put xs >> return x
        else throwError Empty

anyToken :: ParserM s s
anyToken = satisfy (const True)

token :: (Eq s) => s -> ParserM s s
token t = satisfy (== t)

oneOf :: (Eq s) => [s] -> ParserM s s
oneOf ts = satisfy (`elem` ts)

noneOf :: (Eq s) => [s] -> ParserM s s
noneOf ts = satisfy (`notElem` ts)

between :: ParserM s open -> ParserM s close -> ParserM s a -> ParserM s a
between open close p = open *> p <* close

surround :: ParserM s t -> ParserM s a -> ParserM s a
surround t = between t t

chainl1 :: ParserM s a -> ParserM s (a -> a -> a) -> ParserM s a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x

chainl :: ParserM s a -> ParserM s (a -> a -> a) -> a -> ParserM s a
chainl p op a = chainl1 p op <|> return a