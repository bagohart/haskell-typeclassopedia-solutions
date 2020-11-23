import Data.Foldable
import Data.Monoid

-- class Foldable (t :: * -> *) where 
-- fold :: Monoid m => t m => m
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b
--
-- Exercises:
-- 1. fold = foldMap id
-- 2. foldMap f xs = fold (f <$> xs)
-- 3. foldMap f = foldr ((<>) . f) mempty
-- 4. foldr f e = foldMap (Endo
--
-- f :: a -> m -> m
comp = Endo ("lol " ++) <> Endo ("this is weird" ++) <> Endo ("I just keep adding more stuff " ++) <> Endo (++ " but not here")

-- foldMap :: Monoid m => (a -> m) -> t a -> m
foldr8 :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr8 f e xs = appEndo (foldMap (Endo . f) xs) e

-- idea: we have an a. we get a (b -> b).
-- actually we have a whole list of a, so we get [b -> b]
-- we also have a b.
-- so we can apply the last (b -> b) and get a b.
-- We can feed this to the previous functions and finally get a b.
-- So we get bb1 . bb2 . bb3 etc. which we can implement using Endo.
-- foldMap alone is not sufficient, we also need to apply it eventually though.

-- foldMap . foldMap :: (a -> m) -> t1 (t2 a) -> m
-- ^ this folds 2 layers of structure:
twice = (foldMap . foldMap) id [["lol","nope"],["rofl","mao"]]
