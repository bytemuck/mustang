module Mustang.Parser
  ( ParserM (..),
    ParseError (..),
    runParserM,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State

data ParseError
  = Empty
  | NoAlternative
  deriving (Show, Eq)

newtype ParserM s a = ParserM
  { unParserM :: StateT [s] (ExceptT ParseError IO) a
  }
  deriving (MonadState [s], MonadError ParseError)

runParserM :: ParserM s a -> [s] -> IO (Either ParseError (a, [s]))
runParserM (ParserM m) input =
  runExceptT (runStateT m input)

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
