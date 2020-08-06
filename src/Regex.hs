module Regex (Regex (..), emptyR) where

import Data.Sequence (ViewL (..))
import qualified Data.Sequence as Seq
import Prelude

-- `Regex a` represents a regular expression over symbols in `a`
-- So, `Regex Char` would represent regular expressions over characters,
-- while `Regex Bit` would represent regular expressions over the alphabet {0, 1}
--
-- Each regular expression corresponds to a language, which is essentially a set of
-- strings.
data Regex a
  = -- The regular language only accepting the string of that single symbol
    Only a
  | -- A sequence of languages, where each string is followed by a string in the next language
    -- This can also be used to represent the empty language, via Sequenced [].
    -- This is useful so that we don't have multiple equivalent expressions for the same language
    Sequenced (Seq (Regex a))
  deriving (Show)

-- The empty language (over any set of symbols)
emptyR :: Regex a
emptyR = Sequenced mempty

-- Normalizing reduces a regex to the simplest form possible
normalize :: Regex a -> Regex a
normalize (Only a) = Only a
normalize (Sequenced rs) = case Seq.viewl flat of
  r :< sq | Seq.null sq -> r
  _ -> Sequenced flat
  where
    flat = rs >>= (normalize >>> split)
    split :: Regex a -> Seq (Regex a)
    split (Only a) = one (Only a)
    split (Sequenced rs') = rs'

instance Eq a => Eq (Regex a) where
  r1 == r2 = equal (normalize r1) (normalize r2)
    where
      equal (Only a) (Only b) = a == b
      equal (Sequenced rs1) (Sequenced rs2) = all (uncurry equal) (Seq.zip rs1 rs2)
      equal _ _ = False

-- The Semigroup instance here uses concatenation of languages
instance Semigroup (Regex a) where
  r <> Sequenced rs = Sequenced (one r <> rs)
  Sequenced rs <> r = Sequenced (rs <> one r) -- Inefficient, but ok
  r1 <> r2 = Sequenced (one r1 <> one r2)

-- The Empty language acts as the identity for concatenation
instance Monoid (Regex a) where
  mempty = emptyR
