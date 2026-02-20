module Lance.Resolve.Resolve
  ( RExpr (..),
    ResolveM (..),
    resolve,
    resolveMany,
    coreEnvironment,
  )
where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import Data.Map qualified as Map
import Lance.Evaluate.Env (Environment (..), addBinding, lookupEnvironment)
import Lance.Resolve.ResolvedExpr
  ( RExpr (..),
    RPrimitive (RPrimitiveIO, RPrimitivePure),
    RPrimitiveCall (RPrimitiveCallIO, RPrimitiveCallPure),
    RValue (RBoolean, RList, RNumber, RString),
  )
import Lance.Tokenize.TokenizedExpr
  ( LocatedTExpr (LocatedTExpr),
    TAtom (Identifier, Number, String),
    TExpr (Atom, List),
  )

coreEnvironment :: Environment RExpr
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

pBinary :: ([Integer] -> Integer) -> String -> ([RExpr] -> RExpr)
pBinary fn name = \case
  [] -> RResolveError $ name ++ " needs at least one parameter."
  exprs ->
    case traverse extractNumber exprs of
      Left err -> RResolveError err
      Right nums -> RValue (RNumber $ fn nums)
    where
      extractNumber :: RExpr -> Either String Integer
      extractNumber (RValue (RNumber n)) = Right n
      extractNumber _ = Left $ name ++ " can only have numbers as parameters."

pSum :: [RExpr] -> RExpr
pSum = pBinary sum "(+)"

pProduct :: [RExpr] -> RExpr
pProduct = pBinary product "(*)"

pQuotiant :: [RExpr] -> RExpr
pQuotiant = pBinary (foldl1 div) "(/)"

pDifference :: [RExpr] -> RExpr
pDifference = pBinary (foldl1 (-)) "(-)"

pList :: [RExpr] -> RExpr
pList l = RValue $ RList l

pCompare :: (Integer -> Integer -> Bool) -> String -> [RExpr] -> RExpr
pCompare fn name = \case
  [RValue (RNumber l), RValue (RNumber r)] -> RValue $ RBoolean (l `fn` r)
  _ -> RResolveError $ name ++ " can only compare numbers."

pEqual :: [RExpr] -> RExpr
pEqual = pCompare (==) ">"

pGreater :: [RExpr] -> RExpr
pGreater = pCompare (>) ">"

pLess :: [RExpr] -> RExpr
pLess = pCompare (<) ">"

pGreaterEqual :: [RExpr] -> RExpr
pGreaterEqual = pCompare (>=) ">"

pLessEqual :: [RExpr] -> RExpr
pLessEqual = pCompare (<=) ">"

pPrintPrim :: [RExpr] -> IO RExpr
pPrintPrim exprs = do
  putStr $ intercalate ", " (map show exprs)
  return RNil

pPrintfnPrim :: [RExpr] -> IO RExpr
pPrintfnPrim exprs = do
  putStrLn $ intercalate ", " (map show exprs)
  return RNil

pHead :: [RExpr] -> RExpr
pHead [RValue (RList [])] = RNil
pHead [RValue (RList (l : _))] = l
pHead _ = RNil

pTail :: [RExpr] -> RExpr
pTail [RValue (RList [])] = RNil
pTail [RValue (RList (_ : l))] = RValue $ RList l
pTail _ = RNil

pCons :: [RExpr] -> RExpr
pCons [v, RValue (RList r)] = RValue $ RList $ v : r
pCons [v, RNil] = RValue $ RList [v]
pCons _ = RNil

pConcat :: [RExpr] -> RExpr
pConcat [RValue (RList l), RValue (RList r)] = RValue $ RList $ l ++ r
pConcat _ = RNil

pNull :: [RExpr] -> RExpr
pNull [RNil] = RValue $ RBoolean True
pNull [RValue (RList [])] = RValue $ RBoolean True
pNull _ = RValue $ RBoolean False

primitives :: [(String, RExpr)]
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
    ("println", RPrimitive $ RPrimitiveIO "println" pPrintfnPrim),
    ("head", RPrimitive $ RPrimitivePure "head" pHead),
    ("tail", RPrimitive $ RPrimitivePure "tail" pTail),
    (":", RPrimitive $ RPrimitivePure ":" pCons),
    ("++", RPrimitive $ RPrimitivePure "++" pConcat),
    ("null", RPrimitive $ RPrimitivePure "null" pNull)
  ]

resolve :: LocatedTExpr -> ResolveM RExpr RExpr
resolve (LocatedTExpr _ (Atom (Number n))) = return $ RValue $ RNumber n
resolve (LocatedTExpr _ (Atom (String s))) = return $ RValue $ RString s
resolve (LocatedTExpr _ (Atom (Identifier "nil"))) = return RNil
resolve (LocatedTExpr _ (Atom (Identifier name))) = do
  env <- getEnv
  case lookupEnvironment env name of
    Just _ -> return $ RBinding name
    Nothing -> return $ RResolveError $ "Unbound variable '" ++ name ++ "'"
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier "do")) : body))) = do
  newEnv <- getEnv
  bodies <- withEnv newEnv $ mapM resolve body
  return $ RDo bodies
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier "if")) : predicate : truty : [falsy]))) = do
  newEnv <- getEnv
  p <- withEnv newEnv $ resolve predicate
  t <- withEnv newEnv $ resolve truty
  f <- withEnv newEnv $ resolve falsy
  return $ RIf p t f
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier "when")) : predicate : [truty]))) = do
  newEnv <- getEnv
  p <- withEnv newEnv $ resolve predicate
  t <- withEnv newEnv $ resolve truty
  return $ RIf p t RNil
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier "set")) : LocatedTExpr _ (Atom (Identifier name)) : [value]))) = do
  env <- getEnv
  case lookupEnvironment env name of
    Just _ -> do
      resolvedValue <- resolve value
      setEnv $ addBinding env name resolvedValue
      return $ RSet name resolvedValue
    Nothing -> return $ RResolveError $ "Unbound variable '" ++ name ++ "'"
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier "let")) : LocatedTExpr _ (Atom (Identifier name)) : [value]))) = do
  env <- getEnv
  resolvedValue <- resolve value
  setEnv $ addBinding env name resolvedValue
  return $ RLet name resolvedValue
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier "defun")) : LocatedTExpr _ (Atom (Identifier name)) : LocatedTExpr _ (List params) : body))) = do
  let paramNames = [p | LocatedTExpr _ (Atom (Identifier p)) <- params]
  env <- getEnv
  let partialEnv = addBinding env name (RLambda True name paramNames [])
  let lambdaEnv = foldl (\acc p -> addBinding acc p (RParameter p)) partialEnv paramNames
  bodyR <- withEnv lambdaEnv $ mapM resolve body
  let isPure = all (checkPure env) bodyR
  let lambda = RLambda isPure name paramNames bodyR
  setEnv $ addBinding env name lambda
  return lambda
  where
    checkPure oldEnv =
      \case
        (RBinding bindingName) -> case lookupEnvironment oldEnv bindingName of
          Just (RLambda isPure _ _ _) -> isPure
          Just (RPrimitive _) -> True
          Just _ -> False
          Nothing -> True
        (RSet _ _) -> False
        (RPrimitiveCall (RPrimitiveCallIO _ _ rexprs')) -> all (checkPure oldEnv) rexprs'
        (RPrimitiveCall (RPrimitiveCallPure _ _ rexprs')) -> all (checkPure oldEnv) rexprs'
        (RLambdaCall _ rexprs') -> all (checkPure oldEnv) rexprs'
        _ -> True
resolve (LocatedTExpr _ (List (LocatedTExpr _ (Atom (Identifier name)) : args))) = do
  env <- getEnv
  resolvedArgs <- withEnv env $ mapM resolve args
  case lookupEnvironment env name of
    Just (RPrimitive (RPrimitivePure fnName f)) -> return $ RPrimitiveCall $ RPrimitiveCallPure fnName f resolvedArgs
    Just (RPrimitive (RPrimitiveIO fnName f)) -> return $ RPrimitiveCall $ RPrimitiveCallIO fnName f resolvedArgs
    Just (RParameter fnName) -> return $ RLambdaCall fnName resolvedArgs
    Just (RLambda _ lambdaName _ _) -> return $ RLambdaCall lambdaName resolvedArgs
    _ -> return $ RResolveError $ "Could not resolve " ++ name ++ " with " ++ show resolvedArgs
resolve expr = return $ RResolveError $ show expr

resolveMany :: [LocatedTExpr] -> ResolveM RExpr [RExpr]
resolveMany = mapM resolve