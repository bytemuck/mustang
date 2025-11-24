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
import GHC.IO.Handle (Handle, hFlush)
import RExp
import System.Exit
import System.IO (hPutStrLn)
import Text.Pretty.Simple (pPrint)

newtype EvalM a b = EvalM {runEvalM :: (Environment a, Handle) -> IO (Environment a, b)}

instance Functor (EvalM a) where
  fmap :: (b -> c) -> EvalM a b -> EvalM a c
  fmap f (EvalM g) = EvalM $ \env -> do
    (env', a) <- g env
    pure (env', f a)

instance Applicative (EvalM a) where
  pure :: b -> EvalM a b
  pure x = EvalM $ \(env, _) -> pure (env, x)
  (<*>) :: EvalM a (b -> c) -> EvalM a b -> EvalM a c
  (<*>) = ap

instance Monad (EvalM a) where
  (>>=) :: EvalM a b -> (b -> EvalM a c) -> EvalM a c
  (EvalM f) >>= g = EvalM $ \(env, handle) -> do
    (env', x) <- f (env, handle)
    runEvalM (g x) (env', handle)

instance MonadIO (EvalM a) where
  liftIO :: IO b -> EvalM a b
  liftIO action = EvalM $ \(env, _) -> do
    x <- action
    pure (env, x)

getEnv :: EvalM a (Environment a)
getEnv = EvalM $ \(env, _) -> pure (env, env)

setEnv :: Environment a -> EvalM a ()
setEnv newEnv = EvalM $ \_ -> pure (newEnv, ())

withEnv :: Environment a -> EvalM a b -> EvalM a b
withEnv newEnv (EvalM f) = EvalM $ \(_, handle) -> f (newEnv, handle)

getHandle :: EvalM a Handle
getHandle = EvalM $ \(env, handle) -> return (env, handle)

lookupEnvironment' :: Environment RExp -> String -> RExp
lookupEnvironment' env name = Data.Maybe.fromMaybe RUnexpected (lookupEnvironment env name)

isTruthy :: RExp -> Bool
isTruthy (RValue (RBoolean False)) = False
isTruthy (RValue (RNumber n)) = n /= 0
isTruthy RNil = False
isTruthy (RValue (RList xs)) = not (null xs)
isTruthy _ = True

skipStack :: [String]
skipStack = ["tail", "head", "++", ":", "null", "foldl", "map1"]

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
  handle <- getHandle
  env <- getEnv
  case call of
    RPrimitiveCallIO name func params -> do
      let stack = name ++ ": " ++ show params
      evaluated <- withEnv env $ evalMany params
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle stack
      liftIO $ func evaluated
    RPrimitiveCallPure name func params -> do
      let stack = name ++ ": " ++ show params
      evaluated <- withEnv env $ evalMany params
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle stack
      let retState = func evaluated
      return retState
eval l@(RLambda _ name _ _) = do
  env <- getEnv
  setEnv $ addBinding env name l
  return l
eval (RLambdaCall _ name params) = do
  handle <- getHandle
  env <- getEnv
  case lookupEnvironment' env name of
    RLambda _ name' paramNames content -> do
      let stack = name' ++ ": " ++ show params
      evaluatedArgs <- mapM eval params
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle stack
      let lambdaEnv = extendEnvironment env paramNames evaluatedArgs
      evaluatedBody <- withEnv lambdaEnv $ evalMany content
      let retState = last evaluatedBody
      return retState
    RPrimitive (RPrimitiveIO name' fn) -> do
      let stack = name' ++ ": " ++ show params
      evaluatedArgs <- mapM eval params
      liftIO $ hPutStrLn handle stack
      liftIO $ fn evaluatedArgs
    RPrimitive (RPrimitivePure name' fn) -> do
      let stack = name' ++ ": " ++ show params
      evaluatedArgs <- mapM eval params
      liftIO $ hPutStrLn handle stack
      return $ fn evaluatedArgs
    _ -> return RUnexpected
eval e@(RResolveError _) = return e
eval RNil = return RNil
eval e = do
  pPrint $ "(exception): " ++ show e
  liftIO exitFailure

evalMany :: [RExp] -> EvalM RExp [RExp]
evalMany = mapM eval