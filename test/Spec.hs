{-# LANGUAGE BlockArguments #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Regex (Regex (..), emptyR, normalize)
import Test.Hspec
import Prelude

main :: IO ()
main = do
  traverse hspec specs
  traverse checkParallel groups |> void

specs :: [SpecWith ()]
specs = [exampleSpec]

groups :: [Group]
groups = [exampleGroup, regexGroup]

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

regexGroup :: Group
regexGroup =
  Group
    "Regex Properties"
    [ ("sequencing is associative", sequencingAssociative),
      ("id <> r = r", sequencingLeftIdentity),
      ("r <> id = r", sequencingRightIdentity),
      ("normalizing is idempotent", normalizingIdempotent)
    ]

genRegex :: MonadGen m => m a -> m (Regex a)
genRegex genA = Gen.choice [genOnly, genSequence]
  where
    genOnly = Only <$> genA
    genSequence = Sequenced <$> Gen.seq (Range.linear 0 3) (genRegex genA)

sequencingAssociative :: Property
sequencingAssociative = property do
  r1 <- forAll (genRegex Gen.bool)
  r2 <- forAll (genRegex Gen.bool)
  r3 <- forAll (genRegex Gen.bool)
  r1 <> (r2 <> r3) === (r1 <> r2) <> r3

sequencingLeftIdentity :: Property
sequencingLeftIdentity = property do
  r1 <- forAll (genRegex Gen.bool)
  emptyR <> r1 === r1

sequencingRightIdentity :: Property
sequencingRightIdentity = property do
  r1 <- forAll (genRegex Gen.bool)
  r1 <> emptyR === r1

normalizingIdempotent :: Property
normalizingIdempotent = property do
  r <- forAll (genRegex Gen.bool)
  let once = normalize r
  normalize r === r
