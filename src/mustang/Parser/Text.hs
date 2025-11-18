module Mustang.Parser.Text
  ( alpha,
    digit,
    alphaNum,
    string,
    space,
    spaces,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Char (isAlpha, isDigit)
import Mustang.Parser (ParserM)
import Mustang.Parser.Primitive (satisfy, token)

alpha :: ParserM Char Char
alpha = satisfy isAlpha

digit :: ParserM Char Char
digit = satisfy isDigit

alphaNum :: ParserM Char Char
alphaNum = alpha <|> digit

string :: [Char] -> ParserM Char [Char]
string = foldr (\x -> (<*>) ((:) <$> token x)) (pure [])

space :: ParserM Char Char
space = satisfy (`elem` [' ', '\n', '\t', '\r'])

spaces :: ParserM Char String
spaces = many space