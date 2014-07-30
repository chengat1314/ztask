{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Shoe where

import           App
import           AppError
import           Control.Monad           (liftM)
import           Control.Monad.Trans     (liftIO)
import           Controllers.Common
import           Data.Aeson              hiding (json)
import           Data.Aeson.ByteString64
import           Data.Aeson.Types
import qualified Data.ByteString.Char8   as B
import           Data.Monoid             ((<>))
import           Data.Text               (Text, unpack)
import           Models.Shoe
import           Network.HTTP.Types      (created201)
import           Query
import           System.FilePath         ((</>))
import           Views.Shoe
import           Web.PathPieces
import           Web.Scotty.Trans        (ActionT, json, jsonData, param, raise,
                                          status)

viewShoeList :: ActionT AppError WebM ()
viewShoeList = do
  shoes :: [(Text,Shoe)] <- runDb findEntityList
  blaze $ listShoes shoes

viewShoe :: ActionT AppError WebM ()
viewShoe = do
  kid <- param "id"
  shoes :: Maybe Shoe <- runDb $ findEntity kid
  -- Render shoe if found, otherwise throw NotFound exception.
  maybe (raise NotFound) (blaze . viewShoes kid) shoes

-- Controller for Post shoes using JSON
postShoe :: ActionT AppError WebM ()
postShoe = rescueJSON $ do -- Rethrow all exception as JSONRequestError.
  o <- jsonData
  (shoe, img) <- maybe (raise BadRequest) return $ parseMaybe parseData o
  -- Add a new shoes to database and return its id.
  kid <- runDb $ insertEntity shoe
  pDir <- getPhotoDir
  -- Name the shoe's photo the same with its id.
  let sid = unpack $ toPathPiece kid
  let savingImg = pDir </> sid <> ".jpg"
  (liftIO . B.writeFile savingImg . unByteString64) img
  -- Response with status 201 and id for newly created shoes.
  status created201
  json $ object [ "shoes" .= object [ "id" .= sid ] ]
 where
  -- Custom parser to parse JSON into Shoe and Photo.
  parseData :: Object -> Parser (Shoe, ByteString64)
  parseData v = do
    desc <- v .: "description"
    col <- v .: "color"
    sz <- liftM read $ v .: "size"
    img <- v .: "photo"
    return (Shoe desc col sz, img)
