module Main where

import Control.Applicative
import Data.List (group, sort)
import Env
  ( Environment (EmptyEnvironment, ExtendEnvironment),
    frameEnvironment,
  )
import Evaluate (EvalM (runEvalM), evalMany)
import GHC.Base (when)
import Lex
import Mustang.Parser
import RExp
import Resolve
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Pretty.Simple
import UML

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

calculateArgs :: [String] -> (String, Bool)
calculateArgs (path : "showtree" : _) = (path, True)
calculateArgs (path : _) = (path, False)

main :: IO ()
main = do
  args <- getArgs
  let (path, showTree) = calculateArgs args

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

  umlHandle <- openFile "output/uml.txt" WriteMode
  hSetFileSize umlHandle 0

  stackHandle <- openFile "output/stack.txt" WriteMode
  hSetFileSize stackHandle 0

  case parsed of
    Left err -> pPrint $ "Parse error: " ++ show err
    Right (sexps, []) -> do
      (envR, rR) <- runResolveM (resolveMany sexps) (ExtendEnvironment (frameEnvironment stdEnv) EmptyEnvironment)
      when showTree $ pPrint rR

      result <- umlMany (rR, envR, Nothing)

      let (bodies, links) = (concatMap fst' result, concatMap snd' result)

      hPutStrLn umlHandle bodies
      hPutStrLn umlHandle $ concat (rmdups links)
      hFlush stackHandle
      hClose umlHandle

      case collectErrors rR of
        [] -> do
          _ <- runEvalM (evalMany (mapR ++ redR ++ rR)) (coreEnvironment, stackHandle)
          hFlush stackHandle
          hClose stackHandle
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
      (RLambdaCall _ args) -> collectErrors args ++ acc
      (RResolveError _) -> e : acc
      _ -> acc