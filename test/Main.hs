import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "describe dummy" $ do
    it "it dummy" $ do True