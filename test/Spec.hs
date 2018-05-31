import           App
import           AppPrelude
import           Config.AppConfig
import qualified Database.PostgreSQL.Simple as PGS
import           Init
import           Network.Wai
import           Request
import           Test.Hspec
import           Test.Hspec.Wai             (shouldRespondWith, with)

main :: IO ()
main = do
  runSpec

runSpec :: IO ()
runSpec = do
  testFullServer

testFullServer :: IO ()
testFullServer = do
  EnvConfig {..}   <- readEnv
  (config', _) <- setAppConfig runEnv dbEnvConfig []
  hspec $ with (prepServer config') endpointSpec

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


prepServer :: Config -> IO Application
prepServer config' = do
  truncateDatabase (_appDBConn config')
  return $ app config'

truncateDatabase :: DBConn -> IO ()
truncateDatabase dbConn' = do
  conn <- getConnFromPool dbConn'
  let query' = "DO \n\
               \$func$ \n\
               \BEGIN \n\
               \  EXECUTE \n\
               \  (SELECT 'TRUNCATE TABLE ' \n\
               \       || string_agg(quote_ident(schemaname) || '.' || quote_ident(tablename), ', ') \n\
               \       || ' CASCADE' \n\
               \   FROM   pg_tables \n\
               \   WHERE  schemaname = 'public' \n\
               \   AND tablename NOT IN ('license_permission', 'business_permission') \n\
               \   ); \n\
               \END \n\
               \$func$;"
  _ <- liftIO (PGS.execute_ conn query')
  return ()
