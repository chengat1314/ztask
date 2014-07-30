module Data.Aeson.ByteString64 where

import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Base64 as B64
import           Data.Text.Encoding

-- | Wrapper around ByteString to provide auto base64 encode/decode when working on JSON.
newtype ByteString64 = ByteString64 { unByteString64 :: ByteString }

instance ToJSON ByteString64 where
  toJSON (ByteString64 bs) = toJSON (decodeUtf8 $ B64.encode bs)

instance FromJSON ByteString64 where
  parseJSON o =
    parseJSON o >>= either fail (return . ByteString64) . B64.decode . encodeUtf8
