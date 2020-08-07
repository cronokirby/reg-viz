{-# LANGUAGE BlockArguments #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Regex (Regex (..), emptyR, normalize, parse)
import Test.Hspec
import Prelude

main :: IO ()
main = do
  traverse hspec specs
  traverse checkParallel groups |> void

specs :: [SpecWith ()]
specs = [regexSpec]

groups :: [Group]
groups = [regexGroup]

regexSpec :: SpecWith ()
regexSpec = do
  describe "Regex <>" do
    it "should be able to concatenate basic things" do
      Only 'a' <> Only 'b' `shouldBe` Sequenced (fromList [Only 'a', Only 'b'])
      Only 'a' <> Only 'b' <> Only 'c' `shouldBe` Sequenced (fromList [Only 'a', Only 'b', Only 'c'])
  describe "Regex parse" do
    it "should be able to parse basic sequences" do
      parse "ab" `shouldBe` Right (Sequenced (fromList [Only 'a', Only 'b']))

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
