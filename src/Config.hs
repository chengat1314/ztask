{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config where

import           Data.Configurator as C
import           Data.Default
import           Data.Text         (Text)

import           HFlags

-- | Define the command line argument for passing in config file location.
defineFlag "c:config" ("" :: String) "Config file's location"

-- | Config for our app.
data AppConfig = AppConfig
  { port               :: Int
  , dbConnectionString :: Text
  , photoDir           :: FilePath
  }

-- | Default config
instance Default AppConfig where
  def = AppConfig 3000 "example.db" "."

-- | Populate config using config file, fill in missing config with default value.
getConfig :: IO AppConfig
getConfig =
  if null flags_config
    then return def
    else do
      conf <- C.load [C.Required flags_config]
      po <- C.lookupDefault (port def) conf "port"
      dbConn <- C.lookupDefault (dbConnectionString def) conf "dbConnectionString"
      photo <- C.lookupDefault (photoDir def) conf "photoDir"
      return $ AppConfig po dbConn photo
