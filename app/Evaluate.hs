module Evaluate
  ( EvalM (..),
    eval,
    evalMany,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe qualified
import Env
import RExp
import System.Exit
import Text.Pretty.Simple (pPrint)

newtype EvalM a b = EvalM {runEvalM :: Environment a -> IO (Environment a, b)}

instance Functor (EvalM a) where
  fmap :: (b -> c) -> EvalM a b -> EvalM a c
  fmap f (EvalM g) = EvalM $ \env -> do
    (env', a) <- g env
    pure (env', f a)

instance Applicative (EvalM a) where
  pure :: b -> EvalM a b
  pure x = EvalM $ \env -> pure (env, x)
  (<*>) :: EvalM a (b -> c) -> EvalM a b -> EvalM a c
  (<*>) = ap

instance Monad (EvalM a) where
  (>>=) :: EvalM a b -> (b -> EvalM a c) -> EvalM a c
  (EvalM f) >>= g = EvalM $ \env -> do
    (env', x) <- f env
    runEvalM (g x) env'

instance MonadIO (EvalM a) where
  liftIO :: IO b -> EvalM a b
  liftIO action = EvalM $ \env -> do
    x <- action
    pure (env, x)

getEnv :: EvalM a (Environment a)
getEnv = EvalM $ \env -> pure (env, env)

setEnv :: Environment a -> EvalM a ()
setEnv newEnv = EvalM $ \_ -> pure (newEnv, ())

withEnv :: Environment a -> EvalM a b -> EvalM a b
withEnv newEnv (EvalM f) = EvalM $ \_ -> f newEnv

lookupEnvironment' :: Environment RExp -> String -> RExp
lookupEnvironment' env name = Data.Maybe.fromMaybe RUnexpected (lookupEnvironment env name)

isTruthy :: RExp -> Bool
isTruthy (RValue (RBoolean False)) = False
isTruthy (RValue (RNumber n)) = n /= 0
isTruthy RNil = False
isTruthy (RValue (RList xs)) = not (null xs)
isTruthy _ = True

eval :: RExp -> EvalM RExp RExp
eval r@(RValue _) = return r
eval (RBinding binding) = do
  env <- getEnv
  return $ lookupEnvironment' env binding
eval (RDo bodies) = do
  evaluated <- evalMany bodies
  return $ last evaluated
eval (RLet name val) = do
  env <- getEnv
  evaluatedVal <- eval val
  setEnv $ addBinding env name evaluatedVal
  return val
eval (RSet name val) = do
  env <- getEnv
  case lookupEnvironment' env name of
    RUnexpected -> return RUnexpected
    _ -> do
      evaluatedVal <- eval val
      setEnv $ addBinding env name evaluatedVal
      return val
eval (RIf condition truthy falsy) = do
  result <- eval condition
  if isTruthy result
    then eval truthy
    else eval falsy
eval (RPrimitiveCall call) = do
  env <- getEnv
  case call of
    RPrimitiveCallIO _ func params -> do
      evaluated <- withEnv env $ evalMany params
      liftIO $ func evaluated
    RPrimitiveCallPure _ func params -> do
      evaluated <- withEnv env $ evalMany params
      return $ func evaluated
eval l@(RLambda _ name _ _) = do
  env <- getEnv
  setEnv $ addBinding env name l
  return l
eval (RLambdaCall _ name params) = do
  env <- getEnv
  case lookupEnvironment' env name of
    RLambda _ _ paramNames content -> do
      evaluatedArgs <- mapM eval params
      let lambdaEnv = extendEnvironment env paramNames evaluatedArgs
      evaluatedBody <- withEnv lambdaEnv $ evalMany content
      return $ last evaluatedBody
    _ -> return RUnexpected
eval e@(RResolveError _) = return e
eval RNil = return RNil
eval e = do
  pPrint $ "(exception): " ++ show e
  liftIO exitFailure

evalMany :: [RExp] -> EvalM RExp [RExp]
evalMany = mapM eval