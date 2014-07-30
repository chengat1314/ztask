{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import           AppError
import           Config
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Text               (Text)
import           Database.Persist.Sqlite
import           Models.Shoe
import           Web.Scotty.Trans

-- | Global state
data AppState = AppState { config :: AppConfig , pool :: ConnectionPool }

-- | A custom monad that provide a global state for out App.
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar AppState))

-- | A synonym for lift, explicitly for used at the 'WebM' layer
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- | Helper function for retrieving info from global state.
gets :: (AppState -> b) -> WebM b
gets = flip liftM (ask >>= liftIO . readTVarIO)

-- | Helper function for modifying global state.
modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

-- | Create connection pool.
createDbPool :: MonadIO m
             => Text  -- ^ Database connection string
             -> m ConnectionPool
createDbPool = flip createSqlitePool 10

-- | Run SQL action with connection pool from global state.
runDb :: SqlPersistM a -- ^ SQL Action
      -> ActionT AppError WebM a
runDb s = webM $ gets pool >>= liftIO . runSqlPersistMPool s

-- | Run database migration
migrateDb :: ConnectionPool -> IO ()
migrateDb = liftIO . runSqlPersistMPool (runMigration migrateAll)

-- | Get configured photo directory from global state.
getPhotoDir :: MonadTrans t => t WebM String
getPhotoDir = webM $ photoDir `liftM` gets config
