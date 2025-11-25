-- stackShow (RLambdaCall _ name params, env) = name ++ " : [" ++ concatMap (\rexp -> stackShow (rexp, env) ++ ",") params ++ "]"

module Evaluate
  ( EvalM (..),
    eval,
    evalMany,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Maybe qualified
import Env
import GHC.IO.Handle
import RExp
import System.Exit
import System.IO
import Text.Pretty.Simple

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

tabString :: Int -> String
tabString x = replicate x '\t'

stackShow :: (RExp, Int, [RExp], Environment RExp) -> String
stackShow (RValue n, tabs, ea, env) = stackShow' (n, tabs, ea, env)
stackShow (RBinding n, tabs, ea, env) = do
  let v = lookupEnvironment' env n
  stackShow (v, tabs, ea, env)
stackShow (RPrimitiveCall (RPrimitiveCallPure name _ _), tabs, ea, env) = do
  let newParams = zipWith (\x y -> "P" ++ x ++ " = " ++ stackShow (y, tabs, ea, env)) (map show [(1 :: Integer) ..]) ea
  tabString tabs ++ name ++ " : [" ++ intercalate ", " newParams ++ "]"
stackShow (RPrimitiveCall (RPrimitiveCallIO name _ _), tabs, ea, env) = do
  let newParams = zipWith (\x y -> "P" ++ x ++ " = " ++ stackShow (y, tabs, ea, env)) (map show [(1 :: Integer) ..]) ea
  tabString tabs ++ name ++ " : [" ++ intercalate ", " newParams ++ "]"
stackShow (RLambdaCall _ name _, tabs, ea, env) = do
  (newName, names) <- case lookupEnvironment' env name of
    RLambda _ newName paramNames _ -> return (newName, paramNames)
    RPrimitive (RPrimitivePure newName _) -> return (newName, map (\x -> 'P' : show x) [(1 :: Integer) ..])
    RPrimitive (RPrimitiveIO newName _) -> return (newName, map (\x -> 'P' : show x) [(1 :: Integer) ..])
    _ -> return ("", [] :: [String])

  let newParams = zipWith (\x y -> x ++ " = " ++ stackShow (y, tabs, ea, env)) names ea
  tabString tabs ++ newName ++ " : [" ++ intercalate ", " newParams ++ "]"
stackShow (RLambda _ name _ _, _, _, _) = name
stackShow (RPrimitive (RPrimitiveIO name _), _, _, _) = name
stackShow (RPrimitive (RPrimitivePure name _), _, _, _) = name
stackShow (RNil, _, _, _) = "nil"
stackShow _ = "unknown"

stackShow' :: (RValue, Int, [RExp], Environment RExp) -> String
stackShow' (RNumber n, _, _, _) = show n
stackShow' (RString s, _, _, _) = s
stackShow' (RBoolean b, _, _, _) = show b
stackShow' (RList l, tabs, ea, env) = "(" ++ intercalate ", " (map (\rexp -> stackShow (rexp, tabs, ea, env)) l) ++ ")"

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
eval p@(RPrimitiveCall call) = do
  handle <- getHandle
  env <- getEnv
  let stackSize = countScope env
  case call of
    (RPrimitiveCallIO name func params) -> do
      evaluated <- withEnv env $ evalMany params
      let stack = stackShow (p, countScope env, evaluated, env)
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle stack
      retState <- liftIO $ func evaluated
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle (tabString (stackSize + 1) ++ "Returns = " ++ stackShow (retState, stackSize, [], env))
      return retState
    (RPrimitiveCallPure name func params) -> do
      evaluated <- withEnv env $ evalMany params
      let stack = stackShow (p, countScope env, evaluated, env)
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle stack
      let retState = func evaluated
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle (tabString (stackSize + 1) ++ "Returns = " ++ stackShow (retState, stackSize, [], env))
      return retState
eval l@(RLambda _ name _ _) = do
  env <- getEnv
  setEnv $ addBinding env name l
  return l
eval p@(RLambdaCall _ name params) = do
  handle <- getHandle
  env <- getEnv
  let stackSize = countScope env
  case lookupEnvironment' env name of
    RLambda _ _ paramNames content -> do
      evaluatedArgs <- mapM eval params
      let stack = stackShow (p, stackSize, evaluatedArgs, env)
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle stack
      let lambdaEnv = extendEnvironment env paramNames evaluatedArgs
      evaluatedBody <- withEnv lambdaEnv $ evalMany content
      let retState = last evaluatedBody
      unless (name `elem` skipStack) $ liftIO $ hPutStrLn handle (tabString (stackSize + 1) ++ "Returns = " ++ stackShow (retState, stackSize, [], env))
      return retState
    RPrimitive (RPrimitiveIO _ fn) -> do
      evaluatedArgs <- mapM eval params
      let stack = stackShow (p, stackSize, evaluatedArgs, env)
      liftIO $ hPutStrLn handle stack
      retState <- liftIO $ fn evaluatedArgs
      liftIO $ hPutStrLn handle (tabString (stackSize + 1) ++ "Returns = " ++ stackShow (retState, stackSize, [], env))
      return retState
    RPrimitive (RPrimitivePure _ fn) -> do
      evaluatedArgs <- mapM eval params
      let stack = stackShow (p, stackSize, evaluatedArgs, env)
      liftIO $ hPutStrLn handle stack
      let retState = fn evaluatedArgs
      liftIO $ hPutStrLn handle (tabString (stackSize + 1) ++ "Returns = " ++ stackShow (retState, stackSize, [], env))
      return retState
    _ -> return RUnexpected
eval e@(RResolveError _) = return e
eval RNil = return RNil
eval e = do
  pPrint $ "(exception): " ++ show e
  liftIO exitFailure

evalMany :: [RExp] -> EvalM RExp [RExp]
evalMany = mapM eval