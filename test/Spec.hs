import Test.Hspec
import Prelude

main :: IO ()
main = void $ traverse hspec specs

specs :: [SpecWith ()]
specs = [exampleSpec]

exampleSpec :: SpecWith ()
exampleSpec =
  describe "Example" <| do
    it "is just an example test" <| do
      1 `shouldBe` 1
      2 + 2 `shouldBe` 4
