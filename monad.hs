{-# LANGUAGE InstanceSigs #-}

import Control.Monad

-- exercise: Free monad built from Functor f. dafuq?
data Free f a = Var a | Node (f (Free f a))

instance Functor f => Functor (Free f) where 
    fmap g (Var x) = Var $ g x
    fmap g (Node fFfa) = Node $ (fmap . fmap) g fFfa

instance Functor f => Applicative (Free f) where 
    pure = Var
    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    (<*>) (Var g) (Var x) = Var $ g x
    (<*>) (Var g) (Node fFfa) = Node $ (fmap . fmap) g fFfa
    -- (<*>) (Node fFfg) (Var x) = Node $ (fmap . fmap) ($x) fFfg
    -- ^ I don't need this with the next more general thing:
    -- (not sure if it is even identical o_O)
    -- (<*>) (Node fFfg) (Node fFfa) = Node $ undefined
    -- ^ f ab • f a <- this seems a bit impossible, because I need applicative, not functor for exactly this!
    -- so don't take out the functor from the second thingy:
    (<*>) (Node fFfg) hm = Node $ (<*> hm) <$> fFfg
    -- ^ this recursively jumps over the layers of "Node" on the left hand side, until it arrives
    -- at one of the first two cases
    -- that said: dafuq, what is this magic sorcery uuu_UUU

instance Functor f => Monad (Free f) where 
    return = pure

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (>>=) (Var a) g = g a
    (>>=) (Node fFfa) g = Node $ (>>= g) <$> fFfa

-- exercises: join vs >>= / return
-- (>>=) famb ma = join $ famb <$> ma
-- join mma = mma >>= id

-- utility functions
-- 1. liftM = fmap
u1 = liftM (+1) $ Just 1
-- 2. ap = <*>
u2 = ap (Just (+1)) (Just 1)
-- 3. sequence = sequenceA
sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr ((<*>) . (fmap (:))) (return [])
-- sequence' = foldr (\x ys -> ((:) <$> x) <*> ys) (return [])
-- sequence' [] = return []
-- sequence' (m:ms) = (:) <$> m <*> sequence' ms

-- 4. replicateM = sequence . replicate
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n ma = sequence $ replicate n ma

-- 5.
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f xs = sequence' $ map f xs

-- 5.b
forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' = flip mapM'

forLoopIsGreat = forM' [1..5] print

-- 6.
-- (=<<)
reverseBind :: Monad m => (a -> m b) -> m a -> m b
reverseBind = flip (>>=)
rb = print `reverseBind` return "lol"
rb' = (Just . (+2)) `reverseBind` Just 1

-- 7.
-- (>=>)
kleisliFish :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
kleisliFish g h x = g x >>= h

kf = kleisliFish (Just . (+2)) (Just . (*2)) 1

-- (<=<)
revKleisliFish :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
revKleisliFish = flip kleisliFish

rkf = revKleisliFish (Just . (+2)) (Just . (*2)) 1

-- filterM
-- [a] ~> [m Bool] ~> [m a] ~> m [a]
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f [] = return []
filterM' f (x:xs) = do
    res <- f x -- res is Bool
    ys <- filterM' f xs -- xs is [a]
    return $ if res then x:ys else ys

fm   = filterM' (Just . even) [1..10]
fm'  = filterM' (\x -> if x == 10 then Nothing else Just (even x)) [1..9]
fm'' = filterM' (\x -> if x == 10 then Nothing else Just (even x)) [1..10]

-- zipWithM
zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' f [] _ = return []
zipWithM' f _ [] = return []
zipWithM' f (x:xs) (y:ys) = do
    z <- f x y
    zs <- zipWithM' f xs ys
    return (z:zs)

zw    = zipWithM' (\x y -> Just (x+y)) [1..5] [100..107]
zw'   = zipWithM' (\x y -> if y >= 107 then Nothing else Just (x+y)) [1..5] [100..106]
zw''  = zipWithM' (\x y -> if y >= 107 then Nothing else Just (x+y)) [1..5] [100..107]
zw''' = zipWithM' (\x y -> if y >= 107 then Nothing else Just (x+y)) [1..8] [100..107]

-- foldM
-- signature looks like foldl rather than foldr
foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' _ e [] = return e
foldM' f e (x:xs) = do
    y <- foldM' f e xs
    f y x

fo   = foldM' (\y x -> Just (x+y)) 0 [1..5]
fo'  = foldM' (\y x -> if x+y < 20 then Just (x+y) else Nothing) 0 [1..5]
fo'' = foldM' (\y x -> if x+y < 20 then Just (x+y) else Nothing) 0 [1..6]

-- forever
forever' :: Monad m => m a -> m b
forever' ma = do
    ma
    forever' ma

thisWillTakeAWhile = forever' (putStrLn "neeeever")

-- laws
-- exercise: prove  kleisli fish formulation of laws and usual formulation
-- this looks like it could take a while... -> todo

-- do notation
-- there is the fail method for failed pattern matching in do notation. uh oh.

-- todo further reading:
-- "mother of all monads"
-- "why free monads matter"
-- "tour of all haskell monad functions"
-- philipp wadler's paper
-- Monads as containers
-- Monads as computation
-- all about monads
-- you could have invented monads
-- why do monads matter
-- (liste: blog articles on monads)
-- oleg kiselyov: history of IO monad
