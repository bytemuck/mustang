module Mustang.Parser
  ( ParserM (..),
    ParseError (..),
    ParserState (..),
    runParserM,
    getInput,
    setInput,
    updatePosition,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State

data ParseError e
  = UnexpectedEOI Integer Integer
  | UnexpectedToken e Integer Integer
  | NoAlternative
  deriving (Show, Eq)

data ParserState s = ParserState
  { psInput :: [s],
    psLine :: !Integer,
    psCol :: !Integer
  }
  deriving (Show, Eq)

newtype ParserM s a = ParserM
  { unParserM :: StateT (ParserState s) (ExceptT (ParseError s) IO) a
  }
  deriving (MonadState (ParserState s), MonadError (ParseError s))

runParserM :: ParserM s a -> [s] -> IO (Either (ParseError s) (a, [s]))
runParserM (ParserM m) input = do
  let initState = ParserState input 1 1
  result <- runExceptT (runStateT m initState)
  pure $ case result of
    Left e -> Left e
    Right (a, missing) -> Right (a, psInput missing)

instance Functor (ParserM s) where
  fmap :: (a -> b) -> ParserM s a -> ParserM s b
  fmap f (ParserM m) = ParserM $ fmap f m

instance Applicative (ParserM s) where
  pure :: a -> ParserM s a
  pure x = ParserM $ pure x
  (<*>) :: ParserM s (a -> b) -> ParserM s a -> ParserM s b
  (ParserM mf) <*> (ParserM ma) = ParserM $ mf <*> ma

instance Monad (ParserM s) where
  (>>=) :: ParserM s a -> (a -> ParserM s b) -> ParserM s b
  (ParserM ma) >>= f = ParserM $ ma >>= (\a -> let (ParserM mb) = f a in mb)

instance Alternative (ParserM s) where
  empty :: ParserM s a
  empty = ParserM $ StateT $ \_ -> throwError NoAlternative
  (<|>) :: ParserM s a -> ParserM s a -> ParserM s a
  (ParserM p1) <|> (ParserM p2) = ParserM $ StateT $ \s -> ExceptT $ do
    r1 <- runExceptT (runStateT p1 s)
    case r1 of
      Left _ -> runExceptT (runStateT p2 s)
      Right x -> return (Right x)

instance MonadIO (ParserM s) where
  liftIO :: IO a -> ParserM s a
  liftIO io = ParserM $ liftIO io

getInput :: ParserM s [s]
getInput = gets psInput

setInput :: [s] -> ParserM s ()
setInput xs = do
  st <- get
  put st {psInput = xs}

updatePosition :: Char -> Integer -> Integer -> (Integer, Integer)
updatePosition '\n' ln _ = (ln + 1, 1)
updatePosition _ ln col = (ln, col + 1)