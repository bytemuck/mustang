module Main where

import Control.Applicative
import Lex
import Mustang.Parser
import Resolve
import System.IO
import Text.Pretty.Simple
import Prelude hiding (div)

main :: IO ()
main = do
  handle <- openFile "examples/sandbox.hi" ReadMode
  input <- hGetContents handle

  result <- runParserM (many expression) input
  case result of
    Left err -> pPrint $ "Parse error: " ++ show err
    Right (sexps, _) -> do
      pPrint sexps
      (l, r) <- runResolveM (resolveMany sexps) coreEnvironment

      pPrint l
      pPrint r

      return ()