module SExp
  ( SExp (..),
    SAtom (..),
    LocatedSExp (..),
    getPosition,
  )
where

import Control.Monad.State
import Mustang.Parser

data LocatedSExp = LocatedSExp
  { sexpPosition :: Position, -- line, column, maybe filename
    sexp :: SExp
  }
  deriving (Show, Eq)

data SAtom
  = String String
  | Number Integer
  | Identifier String
  deriving (Show, Eq)

data SExp
  = Atom SAtom
  | List [LocatedSExp]
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