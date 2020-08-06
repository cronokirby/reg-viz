{-# LANGUAGE BlockArguments #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Hspec
import Prelude

main :: IO ()
main = do
  traverse hspec specs
  traverse checkParallel groups |> void

specs :: [SpecWith ()]
specs = [exampleSpec]

groups :: [Group]
groups = [exampleGroup]

exampleSpec :: SpecWith ()
exampleSpec =
  describe "Example" do
    it "is just an example test" do
      1 `shouldBe` 1
      2 + 2 `shouldBe` 4

exampleGroup :: Group
exampleGroup = Group "Example Group" [("example property", exampleProperty)]

exampleProperty :: Property
exampleProperty = property do
  b <- forAll Gen.bool
  b === b
