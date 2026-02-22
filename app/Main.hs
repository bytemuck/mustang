module Main where

import Control.Applicative (Alternative (many))
import Lance.Compile.Compile (lowerIR)
import Lance.Evaluate.Env
  ( Environment (EmptyEnvironment, ExtendEnvironment),
    frameEnvironment,
  )
import Lance.Evaluate.Evaluate (EvalM (runEvalM), evalMany)
import Lance.IRCompile.IRCompile (compileProgram)
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
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  [path] <- getArgs

  handle <- openFile path ReadMode
  input <- hGetContents handle

  parsed <- runParserM (many expression) input

  case parsed of
    Left err -> pPrint $ "Parse error: " ++ show err
    Right (sexps, []) -> do
      (_, rR) <- runResolveM (resolveMany sexps) (ExtendEnvironment (frameEnvironment coreEnvironment) EmptyEnvironment)

      let (consts, instrs, labels, _) = compileProgram rR

      -- print line by line, and align arguments
      -- pPrint consts
      -- pPrint labels
      -- pPrint instrs

      let (loweredConsts, loweredInstrs) = lowerIR labels (consts, instrs)

      pPrint loweredConsts
      pPrint loweredInstrs

      case collectErrors rR of
        [] -> do
          _ <- runEvalM (evalMany rR) coreEnvironment
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