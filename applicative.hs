{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Control.Monad

-- Exercise block 1 (tricky) (wtf)
-- show
-- pure f <*> x = pure (flip ($)) <*> x <*> pure f
-- Start from the right.
--      pure (flip ($)) <*> x <*> pure f
--     { Since <*> is infixl, it is equivalent to }
-- =    (pure (flip ($)) <*> x) <*> pure f
--  { interchange law for u = (pure (flip ($)) <*> x) and pure y = pure f
-- =    pure ($ f) <*> (pure (flip ($)) <*> x)
--  { composition from left to right }
-- =    pure (.) <*> pure ($ f) <*> pure (flip ($)) <*> x
--  { homomorphism from left to right }
-- =    pure ((.) ($ f)) <*> pure (flip ($)) <*> x
--  { again }
-- =    pure ((.) ($ f) (flip ($))) <*> x
--  { write . as infix }
-- =    pure (($ f) . (flip ($))) <*> x
--  { magic }
--  =   pure f <*> x
--  as for the magic:
--      ($ f) . (flip ($)) $ a
--  =   ($ f) (((\f -> \x -> \y -> f y x) ($)) a)
--  =   ($ f) ((\x -> \y -> ($) y x) a)
--  =   ($ f) ((\x -> \y -> y x) a)
--  =   ($ f) (\y -> y a)
--  =   (\z -> z $ f) (\y -> y a)
--  =   (\y -> y a) $ f
--  =   (\y -> y a) f
--  =   f a
--  or without the a:
--  Note that
--  (.) = \g -> \h -> \a -> g (h a)
--  So:
--      ($ f) . (flip ($))
--  =   (.) ($ f) (flip ($))
--  =   (\g -> \h -> \a -> g (h a)) ($ f) (flip ($))
--  =   \a -> ($ f) (flip ($) a)
--  =   \a -> (\z -> z $ f) (flip ($) a)
--  =   \a -> (flip ($) a) $ f
--  =   \a -> (\y -> ($) y a) $ f
--  =   \a -> (\y -> y a) f
--  =   \a -> f a
--
--  flip is:
--  flip f = \x -> \y -> f y x
--  or:
--  flip = \f -> \x -> \y -> f y x

newtype ZipList' a = ZipList' { getZipList' :: [a] }

instance Functor ZipList' where 
    fmap g (ZipList' xs) = ZipList' $ fmap g xs
    (<$) a (ZipList' xs) = ZipList' $ fmap (const a) xs

instance Applicative ZipList' where 
    pure :: a -> ZipList' a
    pure = ZipList' . repeat

    (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
    (<*>) (ZipList' gs) (ZipList' xs) = ZipList' (zipWith ($) gs xs)

data Maybe' a = Just' a | Nothing' deriving (Eq, Show)

instance Functor Maybe' where 
    fmap g (Just' x) = Just' $ g x
    fmap _ Nothing' = Nothing'
    (<$) a (Just' b) = Just' a
    (<$) a Nothing' = Nothing' -- because (a <$) = fmap (const a)

instance Applicative Maybe' where 
    pure = Just'
    (<*>) Nothing' _ = Nothing'
    (<*>) _ Nothing' = Nothing'
    (<*>) (Just' gab) (Just' a) = Just' $ gab a
    (<*) Nothing' _ = Nothing'
    (<*) _ Nothing' = Nothing'
    (<*) (Just' a) (Just' b) = Just' a
    (*>) Nothing' _ = Nothing'
    (*>) _ Nothing' = Nothing'
    (*>) (Just' a) (Just' b) = Just' b

-- Utility functions for Applicative
-- liftA
u1 = liftA (+1) (Just 1)

-- liftA2
u2' :: (Applicative f) => f Int -> f Int -> f Int
u2' = liftA2 (+)
u2 = u2' (Just 1) (Just 2)

-- liftA3
u3' :: (Applicative f) => f Int -> f Int -> f Int -> f Int
u3' = liftA3 $ \x -> \y -> \z -> x + y + z
u3 = u3' [1,2] [3,4] [5,6]

-- *>
u4 = getLine *> putStrLn "lol"
u4' = Nothing *> Just "lol"
u4'' = Just "lol" *> Nothing
u4''' = Just "lol" *> Just "nope"

-- <**>
u5 = (putStrLn "aha" >> return 5) <**> (putStrLn "nope" >> return (^2))
u5' = (flip (<*>)) (putStrLn "aha" >> return 5) (putStrLn "nope" >> return (^2))

u5'' = [1,2,3] <**> [(^2),(+2)]
u5''' = (flip (<*>)) [1,2,3] [(^2),(+2)]

-- when -- why is this in Control.Monad???
u6 =  when True (putStrLn "lol")
u6' =  when False (putStrLn "lol")

-- unless
u7 = unless True (getLine >> return ())
u7' = unless False (getLine >> return ())

-- Exercise block
-- implement sequenceAL
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x:xs) = (:) <$> x <*> sequenceAL xs

-- Alternative formulation: Monoidal
-- ex.1: Implement pure and <*> in terms of unit and **
data Maybe'' a = Just'' a | Nothing'' deriving (Eq, Show)

instance Functor Maybe'' where 
    fmap g (Just'' x) = Just'' $ g x
    fmap _ Nothing'' = Nothing''
    (<$) a (Just'' b) = Just'' a
    (<$) a Nothing'' = Nothing'' -- because (a <$) = fmap (const a)

class Functor f => Monoidal f where 
    unit :: f ()
    (**) :: f a -> f b -> f (a,b)

instance Monoidal Maybe'' where 
    unit :: Maybe'' ()
    unit = Just'' ()

    (**) :: Maybe'' a -> Maybe'' b -> Maybe'' (a,b)
    (**) Nothing'' _ = Nothing''
    (**) _ Nothing'' = Nothing''
    (**) (Just'' x) (Just'' y) = Just'' $ (x,y)

instance Applicative Maybe'' where 
    pure :: a -> Maybe'' a
    pure x = const x <$> unit

    (<*>) :: Maybe'' (a -> b) -> Maybe'' a -> Maybe'' b
    (<*>) fab fa = (\(g,x) -> g x) <$> (fab Main.** fa)

-- vice versa:
-- unit = pure ()
-- (**) fa fb = (,) <$> fa <*> fb

-- ex.2: Give Applicative instances with functions
-- 1. f () -> ()
-- 2. f (a,b) -> (f a, f b)
-- which satisfy reasonable laws?
-- 
-- 1. f () -> () always exists, it's just
-- reverseUnit = const ()
-- 2. is a bit trickier. It seems to work for Maybe:
-- reverseStar Nothing = (Nothing,Nothing)
-- reverseStar (Just (x,y)) = (Just x, Just y)
--
-- And reasonable laws?
-- reverseUnit . unit = id :: () -> ()
-- reverseStar . (**') = id ? No! Example:
-- reverseStar .  **' $ (Just 1, Nothing) = reverseStar Nothing = (Nothing, Nothing)
-- But in the other direction:
-- (**') . reverseStar = id
-- Just (1,2) ~> (Just 1, Just 2) ~> Just (1,2)
-- Nothing ~> (Nothing,Nothing) ~> Nothing
--
-- Also this seems obvious:
-- fst . reverseStar (x ** unit) = x
-- snd . reverseStar (unit ** x) = x
-- Maybe this is reasonable... ?_?

-- ex.3: Prove from implementations that the applicative laws and the monoidal laws are equivalent. uh oh.
-- Implementation:
--       pure x = const x <$> unit
--       (<*>) fab fa = (\(g,x) -> g x) <$> (fab Main.** fa)
--    and
--      unit = pure ()
--      (**) fa fb = (,) <$> fa <*> fb
-- Applicative laws:
--      1. pure id <*> v = v
--      2. pure f <*> pure x = pure (f x)
--      3. u <*> pure y = pure ($ y) <*> u
--      4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
--
-- Monoidal laws:
--      A. unit ** v =~ v
--      B. u ** unit =~ u
--      C. u ** (v ** w) =~ (u ** v) ** w
--
--  I. Prove monoidal laws from applicative laws:
--      unit ** v
--      { use implementation of unit }
--  =   (pure ()) ** v
--      { use implementation of ** (not sure if it is broken because of precedence) }
--  =   (,) <$> (pure ()) <*> v
--      { transform in equivalent term to use 2nd app. law }
--  =   pure (,) <*> pure () <*> v
--      { 2nd app law }
--  =   pure ((),) <*> v
--      { this should be true o_O }
--  = ((),) <$> (pure id) <*> v
--      { 1st app law }
--  = ((),) <$> v
--  =~ v
--
--      u ** unit
--      { implementation of unit and ** }
--  =    pure (,) <*> u <*> pure ()
--  = ... ?_?
--  this seems to be more mysterious than instructive atm o_O
