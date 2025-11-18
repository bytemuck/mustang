module Mustang.Parser.Primitive
  ( satisfy,
    anyToken,
    token,
    oneOf,
    noneOf,
    between,
    surround,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Mustang.Parser

satisfy :: (Char -> Bool) -> ParserM Char Char
satisfy p = do
  st <- get
  case psInput st of
    [] -> throwError $ UnexpectedEOI (psLine st) (psCol st)
    (x : xs)
      | p x -> do
          let (line', col') = updatePosition x (psLine st) (psCol st)
          let newState = st {psInput = xs, psLine = line', psCol = col'}

          put newState
          pure x
      | otherwise -> throwError $ UnexpectedToken x (psLine st) (psCol st)

anyToken :: ParserM Char Char
anyToken = satisfy (const True)

token :: Char -> ParserM Char Char
token t = satisfy (== t)

oneOf :: [Char] -> ParserM Char Char
oneOf ts = satisfy (`elem` ts)

noneOf :: [Char] -> ParserM Char Char
noneOf ts = satisfy (`notElem` ts)

between :: ParserM s open -> ParserM s close -> ParserM s a -> ParserM s a
between open close p = open *> p <* close

surround :: ParserM s t -> ParserM s a -> ParserM s a
surround t = between t t