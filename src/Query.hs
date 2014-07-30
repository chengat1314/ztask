{-# LANGUAGE TypeFamilies #-}
-- | Module for abstracting database interaction with Persistent library.
module Query where

import           Control.Monad        (liftM)
import           Data.Text
import           Database.Persist.Sql
import           Web.PathPieces

-- | Search for an entity using Text as key
findEntity :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val) => Text -> m (Maybe val)
findEntity = maybe (return Nothing) get . fmap Key . fromPathPiece

-- | Proxy for Persistent library's insert. Insert a row into a table
insertEntity :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val) => val -> m (Key val)
insertEntity = insert

-- | Get all records of a table
findEntityList :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
               => m [(Text,val)] -- ^ Return list of key, value for each row
findEntityList = fmap refine `liftM` selectList [] []
  where refine s = (toPathPiece $ unKey $ entityKey s, entityVal s)
