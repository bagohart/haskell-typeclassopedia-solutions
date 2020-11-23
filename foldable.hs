import Data.Foldable
import Data.Monoid
import Data.Semigroup

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

-- second exercise block
-- ex.1
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b
toList1 :: Foldable f => f a -> [a]
toList1 = foldMap pure

toList2 :: Foldable f => f a -> [a]
toList2 = foldr (:) []

-- ex.2
foldr5 :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr5 f e xs = foldr f e (toList xs)

-- ex.3
-- fold :: Monoid m => t m => m
-- foldMap :: Monoid m => (a -> m) -> t a -> m
concat1 :: Foldable t => t [a] -> [a]
concat1 = fold

concatMap1 :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap1 f = foldMap f

and1 :: Foldable t => t Bool -> Bool
and1 = getAll . foldMap All

or1 :: Foldable t => t Bool -> Bool
or1 = getAny . foldMap Any

any1 :: Foldable t => (a -> Bool) -> t a -> Bool
any1 f = getAny . foldMap (Any . f)

all1 :: Foldable t => (a -> Bool) -> t a -> Bool
all1 f = getAll . foldMap (All . f)

sum1 :: (Foldable t, Num a) => t a -> a
sum1 = getSum . foldMap Sum

product1 :: (Foldable t, Num a) => t a -> a
product1 = getProduct . foldMap Product

-- maximum, minimum: can fail, so Max and Min are defined in Semigroup, not as a Monoid, no neutral element here.
-- I could hack this together with my own Monoid, where mempty = error "...", but... no.

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x = any1 (==x)

notElem1 :: (Foldable t, Eq a) => a -> t a -> Bool
notElem1 x = not . elem1 x

find1 :: Foldable t => (a -> Bool) -> t a -> Maybe a
find1 p = Data.Monoid.getFirst . foldMap (\x -> if p x then Data.Monoid.First (Just x) else Data.Monoid.First (Nothing))
