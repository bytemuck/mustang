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
withEnv newEnv (ResolveM f) = ResolveM $ \env -> do
  (_, result) <- f newEnv
  pure (env, result)

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
  _ -> RResolveError $ name ++ " can only compare numbers."

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

pHead :: [RExp] -> RExp
pHead [RValue (RList [])] = RNil
pHead [RValue (RList l)] = head l
pHead _ = RNil

pTail :: [RExp] -> RExp
pTail [RValue (RList [])] = RNil
pTail [RValue (RList l)] = RValue $ RList $ tail l
pTail _ = RNil

pCons :: [RExp] -> RExp
pCons [v, RValue (RList r)] = RValue $ RList $ v : r
pCons [v, RNil] = RValue $ RList [v]
pCons _ = RNil

pConcat :: [RExp] -> RExp
pConcat [RValue (RList l), RValue (RList r)] = RValue $ RList $ l ++ r
pConcat _ = RNil

pNull :: [RExp] -> RExp
pNull [RNil] = RValue $ RBoolean True
pNull [RValue (RList [])] = RValue $ RBoolean True
pNull _ = RValue $ RBoolean False

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
    ("printfn", RPrimitive $ RPrimitiveIO "printfn" pPrintfnPrim),
    ("head", RPrimitive $ RPrimitivePure "head" pHead),
    ("tail", RPrimitive $ RPrimitivePure "tail" pTail),
    (":", RPrimitive $ RPrimitivePure ":" pCons),
    ("++", RPrimitive $ RPrimitivePure ":" pConcat),
    ("null", RPrimitive $ RPrimitivePure "null" pNull)
  ]

resolve :: LocatedSExp -> ResolveM RExp RExp
resolve (LocatedSExp _ (Atom (Number n))) = return $ RValue $ RNumber n
resolve (LocatedSExp _ (Atom (Identifier "nil"))) = return RNil
resolve (LocatedSExp _ (Atom (Identifier name))) = do
  env <- getEnv
  case lookupEnvironment env name of
    Just _ -> return $ RBinding name
    Nothing -> return $ RResolveError $ "Unbound variable '" ++ name ++ "'"
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier "do")) : body))) = do
  newEnv <- getEnv
  bodies <- withEnv newEnv $ mapM resolve body
  return $ RDo bodies
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier "if")) : predicate : truty : [falsy]))) = do
  newEnv <- getEnv
  p <- withEnv newEnv $ resolve predicate
  t <- withEnv newEnv $ resolve truty
  f <- withEnv newEnv $ resolve falsy
  return $ RIf p t f
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier "when")) : predicate : [truty]))) = do
  newEnv <- getEnv
  p <- withEnv newEnv $ resolve predicate
  t <- withEnv newEnv $ resolve truty
  return $ RIf p t RNil
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier "set")) : LocatedSExp _ (Atom (Identifier name)) : [value]))) = do
  env <- getEnv
  case lookupEnvironment env name of
    Just _ -> do
      resolvedValue <- resolve value
      setEnv $ addBinding env name resolvedValue
      return $ RSet name resolvedValue
    Nothing -> return $ RResolveError $ "Unbound variable '" ++ name ++ "'"
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier "let")) : LocatedSExp _ (Atom (Identifier name)) : [value]))) = do
  env <- getEnv
  resolvedValue <- resolve value
  setEnv $ addBinding env name resolvedValue
  return $ RLet name resolvedValue
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier "defun")) : LocatedSExp _ (Atom (Identifier name)) : LocatedSExp _ (List params) : body))) = do
  let paramNames = [p | LocatedSExp _ (Atom (Identifier p)) <- params]
  env <- getEnv
  let partialEnv = addBinding env name (RLambda True name paramNames [])
  let lambdaEnv = foldl (\acc p -> addBinding acc p (RParameter p)) partialEnv paramNames
  bodyR <- withEnv lambdaEnv $ mapM resolve body
  let isPure = all (checkPure env) bodyR
  let lambda = RLambda isPure name paramNames bodyR
  setEnv $ addBinding env name lambda
  return lambda
  where
    checkPure oldEnv = \case
      (RBinding bindingName) -> case lookupEnvironment oldEnv bindingName of
        Just _ -> False
        Nothing -> True
      (RSet _ _) -> False
      (RPrimitiveCall (RPrimitiveCallIO _ _ rexprs')) -> all (checkPure oldEnv) rexprs'
      (RPrimitiveCall (RPrimitiveCallPure _ _ rexprs')) -> all (checkPure oldEnv) rexprs'
      (RLambdaCall isPure _ rexprs') -> isPure && all (checkPure oldEnv) rexprs'
      _ -> True
resolve (LocatedSExp _ (List (LocatedSExp _ (Atom (Identifier name)) : args))) = do
  env <- getEnv
  resolvedArgs <- withEnv env $ mapM resolve args
  case lookupEnvironment env name of
    Just (RPrimitive (RPrimitivePure fnName f)) -> return $ RPrimitiveCall $ RPrimitiveCallPure fnName f resolvedArgs
    Just (RPrimitive (RPrimitiveIO fnName f)) -> return $ RPrimitiveCall $ RPrimitiveCallIO fnName f resolvedArgs
    Just (RParameter fnName) -> return $ RLambdaCall False fnName resolvedArgs
    Just (RLambda isPure lambdaName _ _) -> return $ RLambdaCall isPure lambdaName resolvedArgs
    _ -> return $ RResolveError $ "Could not resolve " ++ name ++ " with " ++ show resolvedArgs
resolve expr = return $ RResolveError $ show expr

resolveMany :: [LocatedSExp] -> ResolveM RExp [RExp]
resolveMany = mapM resolve