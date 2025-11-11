module SExp
  ( SExp (..),
    SAtom (..),
  )
where

data SAtom
  = String String
  | Number Integer
  | Identifier String
  deriving (Show, Eq)

data SExp
  = Atom SAtom
  | List [SExp]
  deriving (Show, Eq)
