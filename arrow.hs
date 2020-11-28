{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Arrow
import Control.Monad
import Data.Monoid

-- class Category arr => Arrow arr where -- this means we get id and (.) from Category!
--                                       -- so there is always a neutral arrow and we can compose them
--  arr :: (b -> c) -> (b `arr` c)       -- this looks like we can 'lift' any function into an arrow ?_?
--  first :: (b `arr` c) -> ((b,d) `arr` (c,d)) -- this looks like we can tack on an uneffected value on any arrow
--  second :: (b `arr` c) -> ((d,b) `arr` (d,c)) -- like first, but different order
--  (***) :: (b `arr` c) -> (b' `arr` c') -> ((b,b') `arr` (c,c')) -- looks like we can parallelize two arrows into one?
--  (&&&) :: (b `arr` c) -> (b `arr` c') -> (b `arr` (c,c')) -- looks like we can "fork" two arrows into one?
--
--  minimal: arr, first (and the Category thing)
--
--  task: how to implement first and second in terms of ***?
--  first' bAc = bAc *** id
--  second' bAc = id *** bAc

-- This isn't hard, but let's define all of this for the function instance:
newtype Func a b = Func { runFunc :: a -> b }

instance Category Func where 
    id :: Func a a
    id = Func Prelude.id
    (.) (Func g) (Func h) = Func (g Prelude.. h)

instance Arrow Func where 
    arr :: (b -> c) -> Func b c
    arr g = Func g
    first :: Func b c -> Func (b,d) (c,d)
    first (Func g) = Func $ \(b,d) -> (g b,d)
    second :: Func b c -> Func (d,b) (d,c)
    second (Func g) = Func $ \(d,b) -> (d,g b)
    (***) :: Func b c -> Func b' c' -> Func (b,b') (c,c')
    (***) (Func g) (Func h) = Func $ \(b,b') -> (g b,h b')
    (&&&) :: Func b c -> Func b c' -> Func b (c,c')
    (&&&) (Func g) (Func h) = Func $ \b -> (g b, h b)

-- the instance for Kleisli seems not too different, although the ~ lazy pattern match is probably significant for some law.
-- Still, let's build it:
-- todo
--
-- todo:
-- implement the things
-- do the tutorial on wikibook
-- ??? read more things or something.
