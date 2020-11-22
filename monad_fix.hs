import Data.Maybe
-- import Control.Monad.Fix

-- mfix :: (a -> m a) -> m a

maybeFix :: (a -> Maybe a) -> Maybe a
maybeFix f = do
    x <- maybeFix f
    f x
    -- aka maybeFix >>= f

-- ^ this is infinite recursion, and rather stupid:
-- maybeFix Just = maybeFix Just = maybeFix Just = ...
-- ^ because to compute this expression, >>= is evaluated,
-- and evaluation of >>= means to first decide if the left argument is Nothing or Just y.
-- also this implementation does not depend on Maybe, it could be any Monad.

maybeFix2 :: (a -> Maybe a) -> Maybe a
maybeFix2 f = ma
    where ma = f (fromJust ma)

myFix :: (a -> a) -> a
myFix f =
    let x = f x
     in x

maybeFix3 :: (a -> Maybe a) -> Maybe a
maybeFix3 f = myFix (f . fromJust)

-- maybeFix2 f = f (fromJust ma) = f (fromJust (f (fromJust ma))) = f (fromJust (f (fromJust (f (fromJust ma)))))
-- = ... o_O
-- For f = const Nothing, we get
-- maybeFix2 f = f (fromJust ma) = Nothing
-- so the fromJust doesn't explode.
-- If ma = Nothing, then it means (using the definition of ma) that f (fromJust ma) = Nothing... 
-- which means that the evaluation was successful without exploding... this hurts my brain x_T
--
-- 1. f _ = Nothing (f doesn't look at its argument)
-- => maybeFix2 f = ma = f (fromJust ma) = Nothing
-- 2. f y = Just x (f always outputs Just, x depends on y)
-- => maybeFix2 f = ma = f (fromJust ma) = Just $ (fromJust $ f' <$> ma) = o____O
-- 3. f y = Just x or Nothing (depending on y, f might output Just x or Nothing)
-- => maybeFix2 f = ma = f (fromJust ma) = ... evaluation of fromJust ma necessary
-- => evaluation of ma necessary => recursion, no termination.
-- ... How does case 2 really work ?_?

-- ex.: MonadFix instance for []
-- not sure what this does but maybe I can implement it just looking at the types ?_? ...
listFix :: (a -> [a]) -> [a]
listFix f = la
    where la = concat [f x | x <- la]
          -- this looks wrong. it runs directly in infinite recursion.

listFix2 :: (a -> [a]) -> [a]
listFix2 f = case myFix (f . head) of -- (f . head) :: [a] -> [a]
               [] -> []
               (x:_) -> x : listFix2 (tail . f)

magic = listFix2 $ \b' -> do
    a <- [1,2,snd b']
    b <- [3,4]
    return (a,b)
    -- ... what ...


-- ... ???
-- fix :: (a -> a) -> a
-- class Monad m => MonadFix m where 
-- mfix :: (a -> m a) -> m a
--
-- mfix f = fix (>>= f)
-- (>>= f) :: m a -> m b
-- ^ this yields exactly the broken first try of implementing maybeFix
