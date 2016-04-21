{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SqliteSnaplet where

import Control.Monad.Trans.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.IO.Class
import Database.Persist.Sqlite hiding (get)
import Snap

------------------------------------------------------------------------
-- Voodoo to glue persistent to snap
-- Soostone/snaplet-persistent

newtype PersistState = PersistState { persistPool :: ConnectionPool }

class MonadIO m => HasPersistPool m where
    getPersistPool :: m ConnectionPool

instance HasPersistPool m => HasPersistPool (NoLoggingT m) where
    getPersistPool = runNoLoggingT getPersistPool

instance HasPersistPool (Handler b PersistState) where
    getPersistPool = gets persistPool

instance MonadIO m => HasPersistPool (ReaderT ConnectionPool m) where
    getPersistPool = ask

initSqlite :: SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initSqlite migration = makeSnaplet "connection-pool" "A simple Sqlite connection pool" Nothing $ do
  pool <- liftIO . runStdoutLoggingT $ createSqlitePool ":memory:" 1
  _ <- liftIO $ runNoLoggingT $ runSqlPool migration pool
  return $ PersistState pool

runPersist :: (HasPersistPool m)
           => SqlPersistT (ResourceT (NoLoggingT IO)) b
           -> m b
runPersist action = do
  pool <- getPersistPool
  withPool pool action

withPool :: MonadIO m
         => ConnectionPool
         -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> m a
withPool cp f = liftIO . runNoLoggingT . runResourceT $ runSqlPool f cp
