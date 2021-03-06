module Regex (Regex (..), emptyR, normalize, showRegex, parse) where

import qualified Data.Attoparsec.Text as AP
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
  | -- A choice between many alternate options
    Or (Seq (Regex a))
  | -- A sequence of languages, where each string is followed by a string in the next language
    -- This can also be used to represent the empty language, via Sequenced [].
    -- This is useful so that we don't have multiple equivalent expressions for the same language
    Sequenced (Seq (Regex a))
  deriving (Show)

-- Show a Regex in the way it would be parsed
showRegex :: (a -> Text) -> Regex a -> Text
showRegex f (Only s) = f s
showRegex f (Sequenced rs) = foldMap (showRegex f) rs
showRegex f (Or rs) = fold (Seq.intersperse "|" (fmap (\r -> "(" <> showRegex f r <> ")") rs))

-- The empty language (over any set of symbols)
emptyR :: Regex a
emptyR = Sequenced mempty

-- Normalizing reduces a regex to the simplest form possible
normalize :: Regex a -> Regex a
normalize (Only a) = Only a
normalize (Or rs) = case Seq.viewl flat of
  r :< sq | Seq.null sq -> r
  _ -> Or flat
  where
    flat = rs >>= (normalize >>> split)
    split :: Regex a -> Seq (Regex a)
    split (Or rs') = rs'
    split r = one r
normalize (Sequenced rs) = case Seq.viewl flat of
  r :< sq | Seq.null sq -> r
  _ -> Sequenced flat
  where
    flat = rs >>= (normalize >>> split)
    split :: Regex a -> Seq (Regex a)
    split (Sequenced rs') = rs'
    split r = one r

-- The equality of regexes is based on first normalizing them, then comparing them
--
-- It's possible that we say that two regexes recognizing the same langauge are unequal.
-- This is normal in the sense that two alternate ways of building up a regex shouldn't
-- necessarily be considered equal. For example, is (A|B)(C|D) the same as AC|BD? not really.
-- The normalization process is more about ensuring that implementation details don't leak
-- out into equality, things like the fact that we represented sequencing as a rose tree
-- instead of a binary tree, etc.
instance Eq a => Eq (Regex a) where
  r1 == r2 = equal (normalize r1) (normalize r2)
    where
      equal (Only a) (Only b) = a == b
      equal (Or rs1) (Or rs2) = all (uncurry equal) (Seq.zip rs1 rs2)
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

-- An error for parse errors
newtype ParseError = ParseError Text deriving (Eq, Show)

-- Try and parse a regular expression from a string
--
-- This uses the standard syntax for regexes, with things like a|b?c
parse :: Text -> Either ParseError (Regex Char)
parse input = case AP.parseOnly regexParser input of
  Left err -> err |> toText |> ParseError |> Left
  Right res -> Right (res)

-- A parser for regular expressions
regexParser :: AP.Parser (Regex Char)
regexParser = normalize <$> do
    rs <- AP.sepBy1 term (AP.char '|')
    return (Or (Seq.fromList rs))
  where
    term :: AP.Parser (Regex Char)
    term = do
      rs <- AP.many' atom
      return (Sequenced (Seq.fromList rs))
    atom :: AP.Parser (Regex Char)
    atom = (parensRegex <|> onlyRegex)
    parensRegex :: AP.Parser (Regex Char)
    parensRegex = do
      _ <- AP.char '('
      r <- regexParser
      _ <- AP.char ')'
      return r
    onlyRegex :: AP.Parser (Regex Char)
    onlyRegex = do
      c <- AP.satisfy (AP.notInClass "|()")
      return (Only c)
