module Main where

import Control.Applicative
import Evaluate (EvalM (runEvalM), evalMany)
import Lex
import Mustang.Parser
import RExp
import Resolve
import System.Environment (getArgs)
import System.IO
import Text.Pretty.Simple
import Prelude hiding (div)

main :: IO ()
main = do
  [path] <- getArgs

  handle <- openFile path ReadMode
  input <- hGetContents handle

  parsed <- runParserM (many expression) input

  case parsed of
    Left err -> pPrint $ "Parse error: " ++ show err
    Right (sexps, []) -> do
      (_, rR) <- runResolveM (resolveMany sexps) coreEnvironment

      case collectErrors rR of
        [] -> do
          _ <- runEvalM (evalMany rR) coreEnvironment
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