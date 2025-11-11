module RExp
  ( RExp (..),
    RPrimitiveCall (..),
    RPrimitive (..),
    RValue (..),
  )
where

data RValue
  = RNumber Integer
  | RString String
  | RBoolean Bool
  | RList [RExp]
  deriving (Show, Eq)

data RPrimitive
  = RPrimitiveIO String ([RExp] -> IO RExp)
  | RPrimitivePure String ([RExp] -> RExp)

data RPrimitiveCall
  = RPrimitiveCallIO String ([RExp] -> IO RExp) [RExp]
  | RPrimitiveCallPure String ([RExp] -> RExp) [RExp]

data RExp
  = RValue RValue
  | RBinding String
  | RLet String RExp
  | RSet String RExp
  | RIf RExp RExp RExp
  | RWhen RExp RExp
  | RParameter String
  | RPrimitive RPrimitive
  | RPrimitiveCall RPrimitiveCall
  | RLambda String [String] [RExp]
  | RLambdaCall String [RExp]
  | RResolveError String
  | RNil
  deriving (Show, Eq)

instance Show RPrimitive where
  show :: RPrimitive -> String
  show _ = "<PrimitiveFn>"

instance Eq RPrimitive where
  (==) :: RPrimitive -> RPrimitive -> Bool
  _ == _ = False

instance Show RPrimitiveCall where
  show :: RPrimitiveCall -> String
  show (RPrimitiveCallIO name _ args) = "<PrimitiveFn (" ++ name ++ ") [" ++ show args ++ "]>"
  show (RPrimitiveCallPure name _ args) = "<PrimitiveFn (" ++ name ++ ") [" ++ show args ++ "]>"

instance Eq RPrimitiveCall where
  (==) :: RPrimitiveCall -> RPrimitiveCall -> Bool
  _ == _ = False

instance Show ([RExp] -> RExp) where
  show _ = "<[RExp] -> RExp>"

instance Eq ([RExp] -> RExp) where
  _ == _ = False
