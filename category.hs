{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Monad
import Data.Monoid

-- class Category arr where 
--  id :: a `arr` a
--  (.) :: (b `arr` c) -> (a `arr` b) -> (a `arr` c)

-- class Category cat where 
--  id :: cat a a
--  (.) :: cat b c -> cat a b -> cat a c

newtype Kleisli' m a b = Kleisli' { runKleisli' :: a -> m b }

instance Monad m => Category (Kleisli' m) where 
    id :: Kleisli' m a a
    id = Kleisli' return 
    (.) :: Kleisli' m b c -> Kleisli' m a b -> Kleisli' m a c
    (.) (Kleisli' g) (Kleisli' h) = Kleisli' (h >=> g)
    -- (.) (Kleisli' g) (Kleisli' h) = Kleisli' (\a -> h a >>= g)
    -- (.) (Kleisli' g) (Kleisli' h) = Kleisli' (\a -> h a >>= \b -> g b)

-- It's not obvious to me why this would be very useful. But let's see.
-- Let's try to build some Category instances.
-- Category is a generalization of function composition:
newtype Func a b = Func { runFunc :: a -> b }

instance Category Func where 
    id :: Func a a
    id = Func Prelude.id
    (.) (Func g) (Func h) = Func (g Prelude.. h)

-- this is not possible, Category has kind * -> * -> *
-- newtype Id a = Id a deriving (Eq,Show)
-- instance Category Id where 
--     id = undefined
--     (.) = undefined

-- this is not possible, it needs to be function-y.
-- Simple containers like this don't work, because I can't define the id thing then:
-- newtype Id a b = Id b deriving (Eq,Show)
-- instance Category Id where 
--     id :: Id a a
--     id = undefined -- must create Id with value of type b, not possible!

-- Category can also be viewed as a generalization of Monoid:
newtype Mono m a b = Mono m deriving (Eq,Show)
-- with 2 phantom types because 1 would be lonely :)

instance Monoid m => Category (Mono m) where 
    id :: Mono m a a
    id = Mono mempty
    (.) :: Mono m b c -> Mono m a b -> Mono m a c
    (.) (Mono m) (Mono m') = Mono (m <> m')

m = Mono $ Sum 5
n = Mono $ Sum 7
o = m >>> n
p = m <<< n
-- this should fulfill the laws because the Monoid should fulfill them.

-- What happens if I combine a function with a monoid?
data MonoFunc m a b = MonoFunc { getMono::m
                               , getFunc :: (a -> b)
                               }

instance (Show m) => Show (MonoFunc m a b) where 
    show (MonoFunc m f) = "(" ++ show m ++ ", f)"

instance (Monoid m) => Category (MonoFunc m) where 
    id :: MonoFunc m a a
    id = MonoFunc mempty (\a -> a)

    (.) :: MonoFunc m b c -> MonoFunc m a b -> MonoFunc m a c
    (.) (MonoFunc m g) (MonoFunc n h) = MonoFunc (m <> n) (g Prelude.. h)
    -- id really should be neutral, since the mempty is neutral and (\a -> a) is very neutral, too.
    -- (.) should be associative, since . and <> are both associative.

mf1 = MonoFunc (Sum 5) (+2)
mf1' = mf1 >>> Control.Category.id
mf1'' = Control.Category.id >>> mf1

-- this is pretty contrived, but maybe I could use this to count the number of functions from which
-- a final function was built up using only (.):
basicFunc1 = MonoFunc (Sum 1) (+1)
basicFunc2 = MonoFunc (Sum 1) (+5)
basicFunc3 = MonoFunc (Sum 1) (subtract 8)
composedFunc = basicFunc1 >>> basicFunc2 >>> basicFunc3

-- Similarly, this should be valid, since . and >> are associative
data IOFunc a b = IOFunc { getIO :: IO ()
                         , getFun :: a -> b
                         }

instance Category IOFunc where 
    id :: IOFunc a a
    id = IOFunc (return ()) (\a -> a)
    (.) :: IOFunc b c -> IOFunc a b -> IOFunc a c
    (.) (IOFunc io1 g) (IOFunc io2 h) = IOFunc (io1 >> io2) (g Prelude.. h)

iof1 = IOFunc (putStrLn "apply this function.") (+2)
comp = iof1 >>> iof1 >>> iof1

-- let's try this. It's similarly to the Kleisli' from above, but with even more Monad.
newtype Moo m a b = Moo (m a -> m b)

instance (Monad m) => Category (Moo m) where 
    id :: Moo m a a
    id = Moo Prelude.id
    (.) :: Moo m b c -> Moo m a b -> Moo m a c
    (.) (Moo fmbc) (Moo fmab) = Moo $ fmbc Prelude.. fmab
    -- ok, that was actually boring, because it's just function composition xD
        


-- todo: Some examples here:
-- search for "the category design pattern" on haskellforall.com
-- https://kowainik.github.io/posts/2019-01-14-tomland
-- https://www.reddit.com/r/haskell/comments/ceh5vr/ideas_for_controlcategory_documentation/
