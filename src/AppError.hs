{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}

module AppError where

import           Data.String
import           Data.Text        (Text, pack)
import           Data.Typeable

import           Data.Aeson       (ToJSON, Value, object, toJSON, (.=))
import           Data.List        (isPrefixOf)
import           Web.Scotty.Trans

data AppError = JSONRequestError String
              | NotFound
              | BadRequest
              | Other Text
  deriving Typeable

instance Show AppError where
  show (JSONRequestError t) = "Bad JSON Request: " ++ t
  show NotFound = "This page doesn't exist"
  show BadRequest = "Bad request"
  show (Other t) = "Other: " ++ show t

instance ScottyError AppError where
  stringError s =
    -- In case json request cannot be parse, treat it as Bad Request
    if | "jsonData" `isPrefixOf` s -> BadRequest
       | otherwise -> (Other . pack) s
  showError = fromString . show

instance ToJSON AppError where
  toJSON (JSONRequestError s) = jsonError s
  toJSON NotFound = jsonError "Not Found"
  toJSON BadRequest = jsonError "Bad request"
  toJSON (Other _) = jsonError "Internal Error"

jsonError :: String -> Value
jsonError detail = object [ "error" .= object [ "detail" .= detail ] ]
