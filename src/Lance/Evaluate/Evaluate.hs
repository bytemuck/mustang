module Lance.Evaluate.Evaluate
  ( EvalM (..),
    eval,
    evalMany,
  )
where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe qualified
import Lance.Evaluate.Env
  ( Environment,
    addBinding,
    extendEnvironment,
    lookupEnvironment,
  )
import Lance.Resolve.ResolvedExpr
  ( RExpr
      ( RBinding,
        RDo,
        RIf,
        RLambda,
        RLambdaCall,
        RLet,
        RNil,
        RPrimitive,
        RPrimitiveCall,
        RResolveError,
        RSet,
        RUnexpected,
        RValue
      ),
    RPrimitive (RPrimitiveIO, RPrimitivePure),
    RPrimitiveCall (RPrimitiveCallIO, RPrimitiveCallPure),
    RValue (RBoolean, RList, RNumber),
  )
import System.Exit (exitFailure)
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

lookupEnvironment' :: Environment RExpr -> String -> RExpr
lookupEnvironment' env name = Data.Maybe.fromMaybe RUnexpected (lookupEnvironment env name)

isTruthy :: RExpr -> Bool
isTruthy (RValue (RBoolean False)) = False
isTruthy (RValue (RNumber n)) = n /= 0
isTruthy RNil = False
isTruthy (RValue (RList xs)) = not (null xs)
isTruthy _ = True

eval :: RExpr -> EvalM RExpr RExpr
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
    (RPrimitiveCallIO _ func params) -> do
      evaluated <- withEnv env $ evalMany params
      liftIO $ func evaluated
    (RPrimitiveCallPure _ func params) -> do
      evaluated <- withEnv env $ evalMany params
      let retState = func evaluated
      return retState
eval l@(RLambda _ name _ _) = do
  env <- getEnv
  setEnv $ addBinding env name l
  return l
eval (RLambdaCall name params) = do
  env <- getEnv
  case lookupEnvironment' env name of
    RLambda _ _ paramNames content -> do
      evaluatedArgs <- mapM eval params
      let lambdaEnv = extendEnvironment env paramNames evaluatedArgs
      evaluatedBody <- withEnv lambdaEnv $ evalMany content
      let retState = last evaluatedBody
      return retState
    RPrimitive (RPrimitiveIO _ fn) -> do
      evaluatedArgs <- mapM eval params
      liftIO $ fn evaluatedArgs
    RPrimitive (RPrimitivePure _ fn) -> do
      evaluatedArgs <- mapM eval params
      let retState = fn evaluatedArgs
      return retState
    _ -> return RUnexpected
eval e@(RResolveError _) = return e
eval RNil = return RNil
eval e = do
  pPrint $ "(exception): " ++ show e
  liftIO exitFailure

evalMany :: [RExpr] -> EvalM RExpr [RExpr]
evalMany = mapM eval