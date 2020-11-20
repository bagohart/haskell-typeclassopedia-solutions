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

-- todo: instance monad

-- exercises: join vs >>= / return
-- (>>=) famb ma = join $ famb <$> ma
-- join mma = mma >>= id

-- utility functions

-- todo:
-- read "mother of all monads"
-- read "why free monads matter"
