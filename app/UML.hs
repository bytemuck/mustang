module UML
  ( uml,
    umlMany,
    fst',
    snd',
  )
where

import Env (Environment, lookupEnvironment)
import RExp

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

uml :: (RExp, Environment RExp, Maybe String) -> (String, [String]) -> IO (String, [String], Maybe String)
uml (RLambda _ name params body, env, _) (_, _) = do
  let start = "class \"" ++ name ++ "\" {\n"
  let formatted = foldl1 (++) (map (\n -> "    " ++ n ++ ": param\n") params)
  bodyFormatted <- umlMany (body, env, Just name)
  let end = "}\n\n"

  return (start ++ formatted ++ concatMap fst' bodyFormatted ++ end, map (\n -> "\"" ++ name ++ "\"" ++ " -> " ++ "\"" ++ n ++ "\"") (concatMap snd' bodyFormatted), Just name)
uml (RLet name content, env, p@(Just _)) (bodies, links) = do
  tuple <- uml (content, env, p) (bodies, links)
  return (bodies ++ "    " ++ name ++ ": local\n", snd' tuple ++ links, Just name)
uml (RBinding name, env, Just _) (bodies, links) = do
  case lookupEnvironment env name of
    Just (RLambda _ lambdaName _ _) -> do
      return (bodies, lambdaName : links, Just name)
    _ -> return (bodies, links, Just name)
uml (RLambdaCall _ name@"reduce" params, env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany (params, env, p)
  return (bodies, concatMap snd' tuples ++ links, Just name)
uml (RLambdaCall _ name@"map" params, env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany (params, env, p)
  return (bodies, concatMap snd' tuples ++ links, Just name)
uml (RLambdaCall _ name params, env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany (params, env, p)
  return (bodies, name : concatMap snd' tuples ++ links, p)
uml (RPrimitiveCall (RPrimitiveCallIO name _ params), env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany (params, env, p)
  return (bodies, concatMap snd' tuples ++ links, Just name)
uml (RPrimitiveCall (RPrimitiveCallPure name _ params), env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany (params, env, p)
  return (bodies, concatMap snd' tuples ++ links, Just name)
uml (RDo rexps, env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany (rexps, env, p)
  return (bodies, concatMap snd' tuples ++ links, Just "do")
uml (RIf condition truely falsely, env, p@(Just _)) (bodies, links) = do
  tuples <- umlMany ([condition, truely, falsely], env, p)
  return (bodies, concatMap snd' tuples ++ links, Just "if")
uml (RSet _ content, env, p@(Just _)) (bodies, links) = do
  tuple <- uml (content, env, p) (bodies, links)
  return (bodies, snd' tuple ++ links, Just "set")
uml _ (bodies, links) = return (bodies, links, Nothing)

umlMany :: ([RExp], Environment RExp, Maybe String) -> IO [(String, [String], Maybe String)]
umlMany (rexps, env, parents) = mapM (\rexp -> uml (rexp, env, parents) ("", [])) rexps