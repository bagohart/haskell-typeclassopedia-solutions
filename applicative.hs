{-# LANGUAGE InstanceSigs #-}

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

-- continue: utility functions for Applicative
