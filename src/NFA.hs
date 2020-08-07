module NFA (NFA, Transition, only, sequenced, WriterM) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude

-- Represents a label we can have on an arrow of an NFA
--
-- Either multiple symbols bring us to some place, or we have an empty
-- transition across this arrow, or the rest of the symbols for this state lead some place
data Transition s = Empty | Rest | WithSymbols (NonEmpty s) deriving (Eq, Show)

showTransition :: (s -> Text) -> Transition s -> Text
showTransition _ Empty = "ε"
showTransition _ Rest = ""
showTransition f (WithSymbols (c :| cs)) = foldr (\x acc -> f x <> ", " <> acc) (f c) cs

-- A representation of an NFA representing the process recognizing a language
--
--
-- We represent an NFA with a type of symbols and states as an entrance state,
-- as well as a set of all states, a set of accept states, and a map from each state to its outgoing transitions
data NFA s a = NFA a (Set a) (Set a) (Map a [(Transition s, a)]) deriving (Eq, Show)

-- Create an NFA for a regex only recognizing a string with a single symbol
only :: s -> NFA s Int
only s = NFA 0 (fromList [0, 1, 2]) (fromList [1]) (fromList [(0, [(WithSymbols (one s), 1)]), (1, [(Rest, 2)])])

-- An NFA represented the language brought about by composition
sequenced :: (Ord a, Ord b) => NFA s a -> NFA s b -> NFA s (Either a b)
sequenced (NFA start1 states1 accept1 trans1) (NFA start2 states2 accept2 trans2) =
  let start = Left start1
      states = Set.union (Set.map Left states1) (Set.map Right states2)
      accept = Set.map Right accept2
      changeMap f = fmap (fmap (\(t, a) -> (t, f a))) >>> Map.mapKeys f
      bothTransitions = Map.union (changeMap Left trans1) (changeMap Right trans2)
      extraTransitions = Map.fromList (zip (Left <$> Set.toList accept1) (repeat [(Empty, Right start2)]))
      transitions = Map.unionWith (++) extraTransitions bothTransitions
   in NFA start states accept transitions

simplifyStates :: Ord a => NFA s a -> NFA s Integer
simplifyStates (NFA start states accept trans) =
  NFA (f start) (Set.map f states) (Set.map f accept) (Map.mapKeys f (fmap (\(t, a) -> (t, f a)) <$> trans))
  where
    stateMap = Map.fromList (zip (Set.toList states) [1 ..])
    f s = Map.findWithDefault 0 s stateMap

-- A class for being able to write things
class Monad m => WriterM m where
  write :: Text -> m ()

instance WriterM IO where
  write = putText

edges :: Map a [(b, a)] -> [(a, a, b)]
edges mp = Map.toList mp >>= \(a, pairs) -> fmap (\(b, a') -> (a, a', b)) pairs

writeNFA :: (Ord a, WriterM m) => (s -> Text) -> NFA s a -> m ()
writeNFA f nfa = do
  let NFA start states accept trans = simplifyStates nfa
  write "digraph G {\n"
  write "{\n"
  write "  node[shape=circle, label=\"\"]"
  forM_ (Set.difference states accept) <| \i -> do
    write "s"
    write (show i)
    write "\n"
  write "}\n"
  write "{\n"
  write "  node[shape=doublecircle, label=\"\"]"
  forM_ accept <| \i -> do
    write "s"
    write (show i)
    write "\n"
  write "}\n"
  forM_ (edges trans) <| \(a, a', t) -> do
    write "s"
    write (show a)
    write " -> "
    write "s"
    write (show a')
    write " [label=\""
    write (showTransition f t)
    write "\"]"
    write "\n"
  write "}\n"
  return ()
