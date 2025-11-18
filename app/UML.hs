module UML
  ( uml,
    umlMany,
  )
where

import RExp

uml :: RExp -> (String, [String]) -> IO (String, [String])
uml (RLambda _ name params body) (_, _) = do
  let start = "class " ++ name ++ " {\n"
  let formatted = foldl1 (++) (map (\n -> "    " ++ n ++ ": param\n") params)
  bodyFormatted <- umlMany body
  let end = "}\n"

  return (start ++ formatted ++ concatMap fst bodyFormatted ++ end, map (\n -> name ++ " -> " ++ n) (concatMap snd bodyFormatted))
uml (RLet name _) (bodies, links) = do
  return (bodies ++ "    " ++ name ++ ": local\n", links)
uml (RLambdaCall _ name _) (bodies, links) = do
  return (bodies, name : links)
uml _ (bodies, links) = return (bodies, links)

umlMany :: [RExp] -> IO [(String, [String])]
umlMany = mapM (\rexp -> uml rexp ("", []))