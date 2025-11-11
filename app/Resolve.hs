module Resolve
  ( RExp (..),
    ResolveM (..),
    resolve,
    resolveMany,
    coreEnvironment,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map qualified as Map
import Env
import RExp
import SExp

coreEnvironment :: Environment RExp
coreEnvironment = ExtendEnvironment (Map.fromList primitives) EmptyEnvironment

newtype ResolveM a b = ResolveM {runResolveM :: Environment a -> IO (Environment a, b)}

getEnv :: ResolveM a (Environment a)
getEnv = ResolveM $ \env -> pure (env, env)

setEnv :: Environment a -> ResolveM a ()
setEnv newEnv = ResolveM $ \_ -> pure (newEnv, ())

withEnv :: Environment a -> ResolveM a b -> ResolveM a b
withEnv newEnv (ResolveM f) = ResolveM $ \_ -> f newEnv

instance Functor (ResolveM a) where
  fmap :: (b -> c) -> ResolveM a b -> ResolveM a c
  fmap f (ResolveM g) = ResolveM $ \env -> do
    (env', x) <- g env
    pure (env', f x)

instance Applicative (ResolveM a) where
  pure :: b -> ResolveM a b
  pure x = ResolveM $ \env -> pure (env, x)

  (<*>) :: ResolveM a (b -> c) -> ResolveM a b -> ResolveM a c
  (<*>) = ap

instance Monad (ResolveM a) where
  (>>=) :: ResolveM a b -> (b -> ResolveM a c) -> ResolveM a c
  (ResolveM f) >>= g = ResolveM $ \env -> do
    (env', x) <- f env
    runResolveM (g x) env'

instance MonadIO (ResolveM a) where
  liftIO :: IO b -> ResolveM a b
  liftIO action = ResolveM $ \env -> do
    x <- action
    pure (env, x)

pBinary :: ([Integer] -> Integer) -> String -> ([RExp] -> RExp)
pBinary fn name = \case
  [] -> RResolveError $ name ++ " needs at least one parameter."
  exprs ->
    case traverse extractNumber exprs of
      Left err -> RResolveError err
      Right nums -> RValue (RNumber $ fn nums)
    where
      extractNumber :: RExp -> Either String Integer
      extractNumber (RValue (RNumber n)) = Right n
      extractNumber _ = Left $ name ++ " can only have numbers as parameters."

pSum :: [RExp] -> RExp
pSum = pBinary sum "(+)"

pProduct :: [RExp] -> RExp
pProduct = pBinary product "(*)"

pQuotiant :: [RExp] -> RExp
pQuotiant = pBinary (foldl1 div) "(/)"

pDifference :: [RExp] -> RExp
pDifference = pBinary (foldl1 (-)) "(-)"

pList :: [RExp] -> RExp
pList l = RValue $ RList l

pCompare :: (Integer -> Integer -> Bool) -> String -> [RExp] -> RExp
pCompare fn name = \case
  [RValue (RNumber l), RValue (RNumber r)] -> RValue $ RBoolean (l `fn` r)
  _ -> RResolveError $ name ++ "(>) can only compare numbers."

pEqual :: [RExp] -> RExp
pEqual = pCompare (==) ">"

pGreater :: [RExp] -> RExp
pGreater = pCompare (>) ">"

pLess :: [RExp] -> RExp
pLess = pCompare (<) ">"

pGreaterEqual :: [RExp] -> RExp
pGreaterEqual = pCompare (>=) ">"

pLessEqual :: [RExp] -> RExp
pLessEqual = pCompare (<=) ">"

pPrintPrim :: [RExp] -> IO RExp
pPrintPrim exprs = do
  putStr $ show exprs
  return RNil

pPrintfnPrim :: [RExp] -> IO RExp
pPrintfnPrim exprs = do
  print exprs
  return RNil

primitives :: [(String, RExp)]
primitives =
  [ ("+", RPrimitive $ RPrimitivePure "+" pSum),
    ("*", RPrimitive $ RPrimitivePure "*" pProduct),
    ("/", RPrimitive $ RPrimitivePure "/" pQuotiant),
    ("-", RPrimitive $ RPrimitivePure "-" pDifference),
    ("=", RPrimitive $ RPrimitivePure "=" pEqual),
    (">", RPrimitive $ RPrimitivePure ">" pGreater),
    ("<", RPrimitive $ RPrimitivePure "<" pLess),
    (">=", RPrimitive $ RPrimitivePure ">=" pGreaterEqual),
    ("<=", RPrimitive $ RPrimitivePure "<=" pLessEqual),
    ("list", RPrimitive $ RPrimitivePure "list" pList),
    ("print", RPrimitive $ RPrimitiveIO "print" pPrintPrim),
    ("printfn", RPrimitive $ RPrimitiveIO "printfn" pPrintfnPrim)
  ]

resolve :: SExp -> ResolveM RExp RExp
resolve (Atom (Number n)) = return $ RValue $ RNumber n
resolve (Atom (Identifier name)) = do
  env <- getEnv
  case lookupEnvironment env name of
    Just _ -> return $ RBinding name
    Nothing -> return $ RResolveError $ "Unbound variable '" ++ name ++ "'"
resolve (List (Atom (Identifier "if") : predicate : truty : [falsy])) = do
  p <- resolve predicate
  t <- resolve truty
  f <- resolve falsy
  return $ RIf p t f
resolve (List (Atom (Identifier "when") : predicate : [truty])) = do
  p <- resolve predicate
  t <- resolve truty
  return $ RWhen p t
resolve (List (Atom (Identifier "set") : Atom (Identifier name) : [value])) = do
  env <- getEnv
  case lookupEnvironment env name of
    Just _ -> do
      resolvedValue <- resolve value
      setEnv $ addBinding env name resolvedValue
      return $ RSet name resolvedValue
    Nothing -> return $ RResolveError $ "Unbound variable '" ++ name ++ "'"
resolve (List (Atom (Identifier "let") : Atom (Identifier name) : [value])) = do
  env <- getEnv
  resolvedValue <- resolve value
  setEnv $ addBinding env name resolvedValue
  return $ RLet name resolvedValue
resolve (List (Atom (Identifier "defunc") : Atom (Identifier name) : List params : body)) = do
  let paramNames = [p | Atom (Identifier p) <- params]
  env <- getEnv
  let lambdaEnv = foldl (\acc p -> addBinding acc p (RParameter p)) env paramNames
  bodyR <- withEnv lambdaEnv $ mapM resolve body
  let lambda = RLambda name paramNames bodyR
  setEnv $ addBinding env name lambda
  return lambda
resolve (List (Atom (Identifier name) : args)) = do
  resolvedArgs <- mapM resolve args
  env <- getEnv
  case lookupEnvironment env name of
    Just (RPrimitive (RPrimitivePure fnName f)) -> return $ RPrimitiveCall $ RPrimitiveCallPure fnName f resolvedArgs
    Just (RPrimitive (RPrimitiveIO fnName f)) -> return $ RPrimitiveCall $ RPrimitiveCallIO fnName f resolvedArgs
    Just (RLambda lambdaName _ _) -> return $ RLambdaCall lambdaName resolvedArgs
    _ -> return $ RResolveError $ "Could not resolve " ++ name ++ " with " ++ show resolvedArgs
resolve expr = return $ RResolveError $ show expr

resolveMany :: [SExp] -> ResolveM RExp [RExp]
resolveMany = mapM resolve