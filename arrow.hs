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

-- todo:
-- implement the things
-- do the tutorial on wikibook
-- ??? read more things or something.

newtype Kleisli' m a b = Kleisli' { runKleisli' :: a -> m b }

instance Monad m => Category (Kleisli' m) where 
    id :: Kleisli' m a a
    id = Kleisli' return 
    (.) :: Kleisli' m b c -> Kleisli' m a b -> Kleisli' m a c
    (.) (Kleisli' g) (Kleisli' h) = Kleisli' (h >=> g)
    -- (.) (Kleisli' g) (Kleisli' h) = Kleisli' (\a -> h a >>= g)
    -- (.) (Kleisli' g) (Kleisli' h) = Kleisli' (\a -> h a >>= \b -> g b)
    
instance Monad m => Arrow (Kleisli' m) where 
    arr :: (b -> c) -> Kleisli' m b c
    arr g = Kleisli' $ return Prelude.. g

    first :: Kleisli' m b c -> Kleisli' m (b,d) (c,d)
    first (Kleisli' g) = Kleisli' $ \ ~(b,d) -> do 
                                            c <- g b
                                            return (c,d)
-- the ~ is a lazy pattern match, if the g is e.g. a constant Nothing, then this might make a difference, I guess

    second :: Kleisli' m b c -> Kleisli' m (d,b) (d,c)
    second (Kleisli' g) = Kleisli' $ \ ~(d,b) -> do
                                            c <- g b
                                            return (d,c)

    (***) :: Kleisli' m b c -> Kleisli' m b' c' -> Kleisli' m (b,b') (c,c')
    (***) (Kleisli' g) (Kleisli' h) = Kleisli' $ \ ~(b,b') -> do
                                                        c <- g b
                                                        c' <- h b'
                                                        return (c,c')

    (&&&) :: Kleisli' m b c -> Kleisli' m b c' -> Kleisli' m b (c,c')
    (&&&) (Kleisli' g) (Kleisli' h) = Kleisli' $ \b -> do
                                                    c <- g b
                                                    c' <- h b
                                                    return (c,c')

-- More instances possible?
newtype Mono m a b = Mono m deriving (Eq,Show)
-- with 2 phantom types because 1 would be lonely :)

instance Monoid m => Category (Mono m) where 
    id :: Mono m a a
    id = Mono mempty
    (.) :: Mono m b c -> Mono m a b -> Mono m a c
    (.) (Mono m) (Mono m') = Mono (m <> m')

instance Monoid m => Arrow (Mono m) where 
    arr :: (b -> c) -> Mono m b c
    arr f = Mono mempty
    first (Mono m) = Mono m
    second (Mono m) = Mono m
    (***) (Mono m1) (Mono m2) = Mono $ m1 <> m2
    (&&&) (Mono m1) (Mono m2) = Mono $ m1 <> m2

m1 = Mono (Sum 1)
m2 = Mono (Sum 2)

-- how to implement second, ***, &&& in terms of arr,first,id,(.)? Let's see, shouldn't be too hard...
class Category' cat where 
    id' :: cat a a
    (!.) :: cat b c -> cat a b -> cat a c
    (<<!) :: cat b c -> cat a b -> cat a c
    (<<!) = (!.)
    (>>!) :: cat a b -> cat b c -> cat a c
    (>>!) = flip (!.)

class Category' arro => Arrow' arro where 
    arr' :: (b -> c) -> arro b c
    first' :: (arro b c) -> arro (b,d) (c,d)
    second' :: (arro b c) -> arro (d,b) (d,c)
    second' f = (arr' swap) !. (first' f) !. (arr' swap)
    (**!) :: arro b c -> arro b' c' -> arro (b,b') (c,c')
    (**!) arr1 arr2 = (first' arr1) >>! (second' arr2)
    (&&!) :: arro b c -> arro b c' -> arro b (c,c')
    (&&!) arr1 arr2 = (arr' forkMe) >>! (first' arr1) >>! (second' arr2)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

forkMe :: a -> (a,a)
forkMe x = (x,x)
