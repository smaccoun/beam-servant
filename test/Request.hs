module Request where

import           AppPrelude
import           Data.Aeson              (Value, encode)
import           Data.CaseInsensitive    (mk)
import qualified Data.Text.Lazy.Encoding as L (encodeUtf8)
import           Network.Wai.Test        (SResponse)
import           Test.Hspec.Wai          (WaiSession, request)
import           Network.HTTP.Types.Header (Header)

baseGet :: Text -> [Header] -> WaiSession SResponse
baseGet url headers =
  request (encodeUtf8 "GET") (encodeUtf8 url) headers (L.encodeUtf8 "")

unauthedGet :: Text -> WaiSession SResponse
unauthedGet url = baseGet url []

post :: Text -> Value -> WaiSession SResponse
post path body = do
  request (encodeUtf8 "POST") (encodeUtf8 path) (headers) (encode body)
  where headers = [(mk "Content-Type", encodeUtf8 $ "application/json")]
