import           App
import           AppPrelude
import qualified Data.Text.Lazy.Encoding   as L (encodeUtf8)
import           Init
import           Network.Wai
import           Network.Wai.Test          (SResponse)
import           Test.Hspec
import           Test.Hspec.Wai            (WaiSession, request,
                                            shouldRespondWith, with)

main :: IO ()
main = do
  runSpec

runSpec :: IO ()
runSpec = do
  EnvConfig {..}   <- readEnv
  (config', _) <- setAppConfig runEnv dbEnvConfig []
  let app' = app config'
  hspec $ with (return app') endpointSpec

endpointSpec :: SpecWith Network.Wai.Application
endpointSpec = context "General Endpoint Test" $ do
  describe "health check" $ do
    it "Should return 200 on GET /health" $ do
      baseGet "/health" `shouldRespondWith` 200

  describe "Bad Endpoint" $ do
    it "Should return 404 on fake endpoint" $ do
      baseGet "/thisisabadfakeendpoint" `shouldRespondWith` 404

baseGet :: Text -> WaiSession SResponse
baseGet url =
  request (encodeUtf8 "GET") (encodeUtf8 url) [] (L.encodeUtf8 "")

