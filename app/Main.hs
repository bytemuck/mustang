module Main where

import Control.Applicative (Alternative (many))
import Lance.Evaluate.Env
  ( Environment (EmptyEnvironment, ExtendEnvironment),
    frameEnvironment,
  )
import Lance.Evaluate.Evaluate (EvalM (runEvalM), evalMany)
import Lance.Resolve.Resolve
  ( ResolveM (runResolveM),
    coreEnvironment,
    resolveMany,
  )
import Lance.Resolve.ResolvedExpr
  ( RExpr
      ( RDo,
        RIf,
        RLambda,
        RLambdaCall,
        RLet,
        RPrimitiveCall,
        RResolveError,
        RSet,
        RValue
      ),
    RPrimitiveCall (RPrimitiveCallIO, RPrimitiveCallPure),
    RValue (RList),
  )
import Lance.Tokenize.Lex (expression)
import Lance.Tokenize.Tokenize (runParserM)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  [path] <- getArgs

  handle <- openFile path ReadMode
  input <- hGetContents handle

  mapHandle <- openFile "resources/std/map.lance" ReadMode
  mapInput <- hGetContents mapHandle
  parsedMap <- runParserM (many expression) mapInput

  reduceHandle <- openFile "resources/std/reduce.lance" ReadMode
  reduceInput <- hGetContents reduceHandle
  parsedReduce <- runParserM (many expression) reduceInput

  (stdEnv, mapR, redR) <- case (parsedMap, parsedReduce) of
    (Right mapP, Right redP) -> do
      (mapEnv, mapResolved) <- runResolveM (resolveMany $ fst mapP) coreEnvironment
      (redEnv, redResolved) <- runResolveM (resolveMany $ fst redP) (ExtendEnvironment (frameEnvironment mapEnv) EmptyEnvironment)
      return (redEnv, mapResolved, redResolved)
    _ -> exitFailure

  parsed <- runParserM (many expression) input

  case parsed of
    Left err -> pPrint $ "Parse error: " ++ show err
    Right (sexps, []) -> do
      (_, rR) <- runResolveM (resolveMany sexps) (ExtendEnvironment (frameEnvironment stdEnv) EmptyEnvironment)

      case collectErrors rR of
        [] -> do
          _ <- runEvalM (evalMany (mapR ++ redR ++ rR)) coreEnvironment
          return ()
        e -> do
          pPrint e
          return ()
    Right (_, rest) -> do
      pPrint $ "Could not parse the entire program. Missing: \n" ++ rest

collectErrors :: [RExpr] -> [RExpr]
collectErrors = foldr collect []
  where
    collect e acc = case e of
      (RValue (RList exprs)) -> collectErrors exprs ++ acc
      (RLet _ expr) -> collectErrors [expr] ++ acc
      (RSet _ expr) -> collectErrors [expr] ++ acc
      (RDo b) -> collectErrors b ++ acc
      (RIf c t f) -> collectErrors [c, t, f] ++ acc
      (RPrimitiveCall (RPrimitiveCallIO _ _ args)) -> collectErrors args ++ acc
      (RPrimitiveCall (RPrimitiveCallPure _ _ args)) -> collectErrors args ++ acc
      (RLambda _ _ _ body) -> collectErrors body ++ acc
      (RLambdaCall _ args) -> collectErrors args ++ acc
      (RResolveError _) -> e : acc
      _ -> acc