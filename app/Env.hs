module Env
  ( Environment (..),
    lookupEnvironment,
    addBinding,
  )
where

import Data.Map qualified as Map

type Frame a = Map.Map String a

data Environment a
  = EmptyEnvironment
  | ExtendEnvironment (Frame a) (Environment a)
  deriving (Show)

lookupEnvironment :: Environment a -> String -> Maybe a
lookupEnvironment EmptyEnvironment _ = Nothing
lookupEnvironment (ExtendEnvironment frame parent) variable =
  case value of
    Just result -> Just result
    Nothing -> lookupEnvironment parent variable
  where
    value = Map.lookup variable frame

addBinding :: Environment a -> String -> a -> Environment a
addBinding EmptyEnvironment _ _ = EmptyEnvironment
addBinding (ExtendEnvironment frame parent) name value =
  ExtendEnvironment newFrame parent
  where
    newFrame = Map.insert name value frame