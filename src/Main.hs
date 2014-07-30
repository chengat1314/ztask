{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           App
import           AppError
import           Config
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Controllers.Common
import           Controllers.Shoe
import           HFlags
import           Network.Wai.Middleware.Static
import           Web.Scotty.Trans

main :: IO ()
main = do
  -- Use the HFlags package to automatically bind the config file command line
  -- argument to flags_config
  $initHFlags "Ztask"
  -- Parse the config file
  cfg <- getConfig
  -- Create database connection pool from connection string in config
  cpool <- createDbPool $ dbConnectionString cfg
  -- Run database migration to synchronize the database's tables with our model
  migrateDb cpool
  -- Save config and pool as state so we can refer to it later without
  -- implicitly passing them around
  state <- newTVarIO $ AppState cfg cpool
  let runM m = runReaderT (runWebM m) state
      runActionToIO = runM
  -- Run scotty with the configured port
  scottyT (port cfg) runM runActionToIO app

app :: ScottyT AppError WebM ()
app = do
  -- Get photo directory from config and make scotty serve static photo
  middleware . staticPolicy . addBase =<< getPhotoDir
  -- Set exception handler
  defaultHandler handleEx
  get "/shoes" viewShoeList -- View list of shoes
  get "/shoes/:id" viewShoe -- View a pair of shoes with photo
  post "/shoes" postShoe    -- Post a pair of shoes by json
  notFound $ raise NotFound
