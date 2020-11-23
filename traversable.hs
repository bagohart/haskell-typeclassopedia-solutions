{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable #-}

import Data.Functor.Identity
import Data.Functor.Const

-- exercises, block 1
-- 1. how to turn a tree of lists into a list of trees? (name 2 natural ways)
-- a) every node in the tree has a list
-- -> build a list where every cell in the list is a tree, containing all the values as a tree which previously were in a list:
--    [1,2]
--    /   \
-- [3,4]  [5,6]
-- ~>
-- [1-2, 3-4, 5-6]
-- the tree can be constructed in different ways, i.e. sorted/unsorted, balanced/unbalanced etc.
-- b) build a tree with the same structure as before, but for each node choose only one list entry non-deterministically.
-- this corresponds to the interpretation of lists as non-deterministic
-- c) build a tree like zipWith: if the longest list in the tree has n entries,
-- then build n trees from that with the same structure, and the m-th tree has the m-th entry of each list.
-- This means, a Leaf [x1..xl] will be turned into an Empty if l < m
--
-- I have no idea if any of this qualifies as a 'natural way' :)
--
-- 2. how to turn a list of trees into a tree of lists?
-- a) memorize the path to the node.
-- gather all paths
-- for every path, add all items to its node if there was any tree which had this item.
-- in other words: just merge all the trees into one tree, but don't merge the elements, just put them into a list.
--
-- 3.
-- traverse . traverse :: (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b))
-- ^ map two layers deep, then drag up the f structure two layers.
ex_1_3 :: Maybe [[Int]]
ex_1_3 = (traverse . traverse) Just [[1,2],[3,4]]

ex_1_3' :: [[Maybe Int]]
ex_1_3' = (traverse . traverse) (\x -> [x,x+1]) [Just 2,Just 8]

ex_1_3'' :: [[Maybe Int]]
ex_1_3'' = (traverse . traverse) (\x -> [x,x+1]) [Just 2, Nothing,Just 8]

-- 4. this is ... no different from the last attempt except no additional Functor constraint is necessary
-- because it is already contained in the "Applicative f"
traverse1 :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse1 f ta = sequenceA $ f <$> ta

sequenceA1 :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA1 = traverse id


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

tree1 :: Tree Int
tree1 = Node (Leaf 1) 5 (Node (Empty) 10 (Leaf 7))

tree2 :: Tree [Int]
tree2 = Node (Leaf [1]) [5] (Node (Empty) [10] (Leaf [7]))

tree3 :: Tree [Int]
tree3 = Node (Leaf [1,2]) [5,6] (Node (Empty) [10,11] (Leaf [7,8]))

listt :: [Tree Int]
listt = [
    tree1,
    (+1) <$> tree1
        ]

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b) 
  traverse g Empty        = pure Empty
  traverse g (Leaf x)     = Leaf <$> g x
  traverse g (Node l x r) = Node <$> traverse g l
                                 <*> g x
                                 <*> traverse g r

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap     g Empty        = Empty
  fmap     g (Leaf x)     = Leaf $ g x
  fmap     g (Node l x r) = Node (fmap g l)
                                 (g x)
                                 (fmap g r)

-- exercises block 2
-- 1.
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
fmap1 :: Traversable f => (a -> b) -> f a -> f b
fmap1 f = runIdentity . traverse (Identity . f)
-- idea: traversing Identity doesn't change anything, so making fmap more general and now more special without
-- effects should be fmap again

foldMap1 :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap1 f = getConst . traverse (Const . f)

-- idea: from t a obtain an t F m, then go to F t m and remove F.
-- the trick: we use Const to save the values, but traverse acts on the phantom values,
-- i.e. the values with the t structure are thrown away, while the rest is combined monoidally.
-- this... looks a bit crazy.
--
-- 2.
-- instance Traversable [] where 
-- sequenceA [] = pure []
-- sequenceA (x:xs) = (:) <$> pure x <*> sequenceA xs
--
-- instance Traversable Maybe where 
-- sequenceA Nothing = pure Nothing
-- sequenceA (Just x) = Just <$> x
--
-- instance Traversable ((,) e) where 
-- sequenceA (c,x) = (c,) <$> x
--
-- instance Traversable (Either e) where 
-- sequenceA (Left l) = pure $ Left l
-- sequenceA (Right r) = Right <$> r
--
-- 3.
-- Foldable: take all the elements and <> them together. Assume some order, then this is like folding a list.
-- Traversable: Set (Maybe Int) ~> Maybe (Set Int)
-- ^ this would yield a Nothing if the Set contains a Nothing, otherwise it contains all its values in a single Set.
-- So this seems reasonable at first glance. Also it's almost like List, so what gives?
-- Answer: Traversable means it must be a Functor, but it's not a Functor, because that would preclude type constraints which are needed for Set
--
-- 4.
data Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show, Functor, Foldable)
--
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
--
instance (Traversable f, Traversable g) => Traversable (Compose f g) where 
    sequenceA :: Applicative h => Compose f g (h a) -> h (Compose f g a)
    sequenceA = (fmap Compose) . sequenceA . (fmap sequenceA) . getCompose
    -- this looks magical, but deriving it by looking at the types was surprisingly straightforward o_O:
    -- Compose $ f (g (h a)) ~> f (g (h a)) ~> f (h (g a)) ~> h (f (g a)) ~> h (Compose f g a)

    traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse fahb = (fmap Compose) . (traverse . traverse) fahb . getCompose
    -- f (g a) ~> f (g (h b)) ~> ...
    -- derive this by looking at the signature of (traverse . traverse) :)

