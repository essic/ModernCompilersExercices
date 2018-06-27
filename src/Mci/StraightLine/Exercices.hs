module Mci.StraightLine.Exercices where

import qualified Mci.Prelude as P

data Tree a = Leaf | Tree (Tree a) a (Tree a)
  deriving P.Show

empty :: (P.Ord key, P.Show key) => Tree key
empty = Leaf

insert :: (P.Ord key, P.Show key) => key -> Tree key-> Tree key
insert key Leaf = Tree empty key empty
insert key (Tree left nodeKey right)
  | key P.< nodeKey = Tree (insert key left) nodeKey right
  | key P.> nodeKey = Tree left nodeKey (insert key right)
  | P.otherwise = Tree left key right

member :: (P.Ord key,P.Show key) => key -> Tree key -> P.Bool
member _ Leaf =
  P.False

member key (Tree left nodeKey right)
  | key P.< nodeKey = member key left
  | key P.> nodeKey = member key right
  | P.otherwise = P.True

