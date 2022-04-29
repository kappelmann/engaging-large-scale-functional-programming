{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Pause (PauseT, pause, stepPause, runPauseT, runPause, stepPauseT) where

import "base" Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Bifunctor

newtype PauseT m a = PauseT {stepPauseT' :: m (Either (PauseT m a) a)}
type Pause = PauseT Identity

instance (Monad m) => Functor (PauseT m) where
  fmap = liftM

instance (Monad m) => Monad (PauseT m) where
  return x = lift (return x)
  (PauseT s) >>= f = PauseT $ s >>= either (return . Left . (>>= f)) (stepPauseT' . f)

instance (Functor m, Monad m) => Applicative (PauseT m) where
  pure  = return
  (<*>) = ap

instance MonadTrans PauseT where
  lift k = PauseT (liftM Right k)
  
instance MonadError e m => MonadError e (PauseT m) where
  throwError = lift . throwError
  catchError m f = PauseT $ catchError (stepPauseT' m) (stepPauseT' . f)

instance MonadState s m => MonadState s (PauseT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader r m => MonadReader r (PauseT m) where
  ask = lift ask
  local f m = PauseT $ local f $ stepPauseT' m

instance MonadWriter w m => MonadWriter w (PauseT m) where
  tell = lift . tell
  listen m = PauseT $ liftM (\(a,w) -> bimap (fmap (,w)) (,w) a) (listen (stepPauseT' m))
  pass m = PauseT $ pass $ do x <- stepPauseT' m
                              return (bimap (fmap fst) fst x, either (const id) snd x)

runPauseT :: Monad m => PauseT m a -> m a
runPauseT m = stepPauseT' m >>= either runPauseT return

runPause :: Pause a -> a
runPause = runIdentity . runPauseT

stepPause :: Pause a -> Either (Pause a) a
stepPause = runIdentity . stepPauseT'

class Monad m => MonadPause m where
  pause :: m ()
  stepPauseT :: m a -> m (Either (m a) a)

instance Monad m => MonadPause (PauseT m) where
  pause = PauseT (return (Left (return ())))
  stepPauseT = lift . stepPauseT'

instance (MonadPause m, Monoid s) => MonadPause (WriterT s m) where
  pause = lift pause
  stepPauseT = mapWriterT $ liftM (either ((,mempty) . Left . WriterT) (first return)) . stepPauseT

instance (MonadPause m) => MonadPause (ReaderT s m) where
  pause = lift pause
  stepPauseT = mapReaderT $ liftM (either (Left . lift) Right) . stepPauseT

instance (MonadPause m) => MonadPause (ExceptT e m) where
  pause = lift pause
  stepPauseT = mapExceptT $ liftM (either (Right . Left . ExceptT) (second Right)) . stepPauseT

