module Lance.Resolve.ResolvedExpr
  ( RExpr (..),
    RPrimitiveCall (..),
    RPrimitive (..),
    RValue (..),
  )
where

import Data.List (intercalate)

-- This module defines the data structures for R(esolved)-expressions, which are an intermediate representation of code in the Lance language.
-- R-expressions can represent values, variable bindings, control flow constructs like if statements, primitive operations, lambda functions, and more.

data RValue
  = RNumber Integer
  | RString String
  | RBoolean Bool
  | RList [RExpr]
  deriving (Show, Eq)

data RPrimitive
  = RPrimitiveIO String ([RExpr] -> IO RExpr)
  | RPrimitivePure String ([RExpr] -> RExpr)

data RPrimitiveCall
  = RPrimitiveCallIO String ([RExpr] -> IO RExpr) [RExpr]
  | RPrimitiveCallPure String ([RExpr] -> RExpr) [RExpr]

data RExpr
  = RValue RValue
  | RBinding String
  | RLet String RExpr
  | RSet String RExpr
  | RIf RExpr RExpr RExpr
  | RPrimitive RPrimitive
  | RPrimitiveCall RPrimitiveCall
  | RLambda Bool String [String] [RExpr]
  | RLambdaCall String [RExpr]
  | RResolveError String
  | RDo [RExpr]
  | RNil
  | RParameter String
  | RUnexpected
  deriving (Eq)

instance Show RExpr where
  show :: RExpr -> String
  show RNil = "nil"
  show RUnexpected = "<unexpected>"
  show (RValue (RString n)) = n
  show (RValue (RNumber n)) = show n
  show (RValue (RBoolean n)) = show n
  show (RValue (RList p)) = "(" ++ intercalate ", " (map show p) ++ ")"
  show (RLambda _ name _ _) = "<lambda: " ++ name ++ ">"
  show (RPrimitive (RPrimitivePure name _)) = "<primitive: " ++ name ++ ">"
  show (RPrimitive (RPrimitiveIO name _)) = "<primitive: " ++ name ++ ">"
  show (RResolveError err) = "<resolve error: " ++ err ++ ">"
  show _ = "<?: unknown>"

instance Eq RPrimitive where
  (==) :: RPrimitive -> RPrimitive -> Bool
  (==) (RPrimitiveIO name _) (RPrimitiveIO name' _) = name == name'
  (==) (RPrimitivePure name _) (RPrimitivePure name' _) = name == name'
  (==) (RPrimitiveIO name _) (RPrimitivePure name' _) = name == name'
  (==) (RPrimitivePure name _) (RPrimitiveIO name' _) = name == name'

instance Eq RPrimitiveCall where
  (==) :: RPrimitiveCall -> RPrimitiveCall -> Bool
  _ == _ = False

instance Eq ([RExpr] -> RExpr) where
  (==) :: ([RExpr] -> RExpr) -> ([RExpr] -> RExpr) -> Bool
  _ == _ = False
