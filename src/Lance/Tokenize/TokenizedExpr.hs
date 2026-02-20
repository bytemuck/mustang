module Lance.Tokenize.TokenizedExpr
  ( TExpr (..),
    TAtom (..),
    LocatedTExpr (..),
    getPosition,
  )
where

import Control.Monad.State (MonadState (get))
import Lance.Tokenize.Tokenize (ParserM, ParserState (psCol, psLine))

-- This module defines the data structures for S-expressions, which are a common way to represent code in Lisp-like languages.
-- Each S-expression can be an atom (like a string, number, or identifier) or a list of other S-expressions.
-- The LocatedTExpr type includes position information for error reporting and debugging purposes.

data LocatedTExpr = LocatedTExpr
  { texpPosition :: Position,
    texp :: TExpr
  }
  deriving (Show, Eq)

data TAtom
  = String String
  | Number Integer
  | Identifier String
  deriving (Show, Eq)

data TExpr
  = Atom TAtom
  | List [LocatedTExpr]
  deriving (Show, Eq)

data Position = Position
  { line :: !Integer,
    col :: !Integer
  }
  deriving (Show, Eq)

getPosition :: ParserM s Position
getPosition = do
  st <- get
  pure (Position (psLine st) (psCol st))