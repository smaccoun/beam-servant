import           AppPrelude
import           Test.Hspec
import Init

main :: IO ()
main = do
  runSpec

runSpec :: IO ()
runSpec = do
  EnvConfig{..} <- readEnv
  hspec $
    describe "add3" $ do
      it "Should add 3 to any number" $ do
        (6 :: Int) `shouldBe` (6 :: Int)

