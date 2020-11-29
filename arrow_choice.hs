{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Arrow
import Control.Monad
import Data.Monoid

-- class Arrow arr => ArrowChoice arr where 
--  left :: (b `arr` c) -> (Either b d `arr` Either c d) -- builds a new ArrowChoice where the left value is the old Arrow
--  right :: (b `arr` c) -> (Either d b `arr` Either d c) -- like left, but right :)
--  (+++) :: (b `arr` c) -> (b' `arr` c') -> (Either b b' `arr` Either c c') -- chooses the 'right' Arrow ? o_O
--  (|||) :: (b `arr` d) -> (c `arr` d) -> (Either b c `arr` d) -- like +++, but the result type is the same, so...
--  This looks very like Arrow, but on sum types instead of product types

-- todo:
-- left and +++ is minimal, so define right and ||| in their terms.
-- define instances for (->) and Kleisli

-- instance for ->
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

-- let's try here to implement right and ||| in terms of left and +++
instance ArrowChoice Func where 
    left :: Func b c -> Func (Either b d) (Either c d)
    left (Func g) = Func $ either (Left Prelude.. g) (Right Prelude.. Prelude.id)

    right :: Func b c -> Func (Either d b) (Either d c)
    right f = swapEither <<< left f <<< swapEither
        where swapEither :: Func (Either b c) (Either c b)
              swapEither = Func $ either Right Left

    (+++) :: Func b c -> Func b' c' -> Func (Either b b') (Either c c')
    (+++) (Func g) (Func h) = Func $ \e -> either (\b -> Left $ g b) (\b' -> Right $ h b') e

    (|||) :: Func b d -> Func c d -> Func (Either b c) d
    (|||) g h = (g +++ h) >>> arr (either Prelude.id Prelude.id)



-- instance for MonoFunc
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

instance (Monoid m) => Arrow (MonoFunc m) where 
    arr :: (b -> c) -> MonoFunc m b c
    arr g = MonoFunc mempty g
    first :: MonoFunc m b c -> MonoFunc m (b,d) (c,d)
    first (MonoFunc m g) = MonoFunc m $ \(b,d) -> (g b,d)

instance (Monoid m) => ArrowChoice (MonoFunc m) where 
    left :: MonoFunc m b c -> MonoFunc m (Either b d) (Either c d)
    left (MonoFunc m g) = MonoFunc m $ either (Left Prelude.. g) (Right)

    (+++) :: MonoFunc m b c -> MonoFunc m b' c' -> MonoFunc m (Either b b') (Either c c')
    (+++) (MonoFunc m g) (MonoFunc m' h) = MonoFunc (m <> m') $ either (Left Prelude.. g) (Right Prelude.. h)
    -- ^^ is this a reasonable behaviour for the Monoid thing? maybe.
    -- Maybe I should quickcheck those laws :)

-- Aaaaand the Kleisli instance
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

instance Monad m => ArrowChoice (Kleisli' m) where 
    left :: Kleisli' m b c -> Kleisli' m (Either b d) (Either c d)
    left (Kleisli' g) = Kleisli' $ either ((fmap Left) Prelude.. g) (return Prelude.. Right)

    (+++) :: Kleisli' m b c -> Kleisli' m b' c' -> Kleisli' m (Either b b') (Either c c')
    (+++) (Kleisli' g) (Kleisli' h) = Kleisli' $ either ((fmap Left) Prelude.. g) ((fmap Right) Prelude.. h)
