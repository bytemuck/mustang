module Main where

import Control.Applicative
import Env
  ( Environment (EmptyEnvironment, ExtendEnvironment),
    frameEnvironment,
  )
import Evaluate (EvalM (runEvalM), evalMany)
import Lex
import Mustang.Parser
import RExp
import Resolve
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Pretty.Simple
import UML
import Prelude hiding (div)

main :: IO ()
main = do
  [path] <- getArgs

  handle <- openFile path ReadMode
  input <- hGetContents handle

  mapHandle <- openFile "resources/std/map.mu" ReadMode
  mapInput <- hGetContents mapHandle
  parsedMap <- runParserM (many expression) mapInput

  reduceHandle <- openFile "resources/std/reduce.mu" ReadMode
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

      -- plant uml with 'rR'

      result <- umlMany rR
      let (bodies, links) = (concatMap fst result, concat $ concatMap snd result)

      pPrint bodies
      pPrint links

      case collectErrors rR of
        [] -> do
          _ <- runEvalM (evalMany (mapR ++ redR ++ rR)) coreEnvironment
          return ()
        e -> do
          pPrint e
          return ()
    Right (_, rest) -> do
      pPrint $ "Could not parse the entire program. Missing: \n" ++ rest

collectErrors :: [RExp] -> [RExp]
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
      (RLambdaCall _ _ args) -> collectErrors args ++ acc
      (RResolveError _) -> e : acc
      _ -> acc