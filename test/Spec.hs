import           App
import           AppPrelude
import           Init
import           Network.Wai
import           Request
import           Test.Hspec
import           Test.Hspec.Wai            (shouldRespondWith, with)

main :: IO ()
main = do
  runSpec

runSpec :: IO ()
runSpec = do
  EnvConfig {..}   <- readEnv
  (config', _) <- setAppConfig runEnv dbEnvConfig []
  let app' = app config'
  hspec $ with (return app') (endpointSpec)

endpointSpec :: SpecWith Network.Wai.Application
endpointSpec = context "General Endpoint Test" $ do
  describe "health check" $ do
    it "Should return 200 on GET /health" $ do
      unauthedGet "/health" `shouldRespondWith` 200

  describe "Bad Endpoint" $ do
    it "Should return 404 on fake endpoint" $ do
      unauthedGet "/thisisabadfakeendpoint" `shouldRespondWith` 404

  describe "Should return unauthorized" $ do
    it "Should return 401 on unauthorized request" $ do
      unauthedGet "/users" `shouldRespondWith` 401
