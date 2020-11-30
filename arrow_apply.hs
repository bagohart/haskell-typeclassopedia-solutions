{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Arrow
import Control.Monad
import Data.Monoid

-- class Arrow arr -> ArrowApply arr where 
--  app :: (b `arr` c, b) `arr` c
--  ^ this looks weird. apparently, the arrow in the first argument is applied to the value in the first argument. hm.

-- instance for (->)
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

instance ArrowApply Func where 
    app :: Func (Func b c,b) c
    app = Func $ \((Func g),b) -> g b

app2' :: Func b (Func (Func b c) c)
app2' = Func $ \b -> Func $ \(Func g) -> g b

-- exercise: now use app, but not specific properties of Func to implement this.
app2'' :: ArrowApply ar => ar b (ar (ar b c) c)
app2'' = arr $ \b -> ((arr $ \g -> (g,b)) >>> app)

-- exercise: Kleisli is an ArrowApply
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

instance Monad m => ArrowApply (Kleisli' m) where 
    app :: Kleisli' m (Kleisli' m b c, b) c
    app = Kleisli' $ \(Kleisli' g,b) -> g b
    -- app = Kleisli' $ \x -> runKleisli' (fst x) $ snd x

newtype ArrowMonad' a b = ArrowMonad' (a () b)

instance ArrowApply a => Functor (ArrowMonad' a) where 
    fmap :: (b -> c) -> ArrowMonad' a b -> ArrowMonad' a c
    fmap g (ArrowMonad' a) = ArrowMonad' $ a >>> (arr g)

instance ArrowApply a => Applicative (ArrowMonad' a) where 
    pure :: b -> ArrowMonad' a b
    pure x = ArrowMonad' $ arr $ \() -> x

    (<*>) :: ArrowMonad' a (b -> c) -> ArrowMonad' a b -> ArrowMonad' a c
    (<*>) (ArrowMonad' g) (ArrowMonad' b) = ArrowMonad' $ (g &&& b) >>> (arr $ \(g,b) -> g b)

instance ArrowApply a => Monad (ArrowMonad' a) where 
    return :: b -> ArrowMonad' a b
    return = pure

    (>>=) :: ArrowMonad' a b -> (b -> ArrowMonad' a c) -> ArrowMonad' a c
    (>>=) (ArrowMonad' b) g = ArrowMonad' $ b >>> ((arr (runArrowMonad' Prelude.. g)) &&& (arr (const ()))) >>> app

runArrowMonad' :: ArrowMonad' a b -> a () b
runArrowMonad' (ArrowMonad' a) = a
