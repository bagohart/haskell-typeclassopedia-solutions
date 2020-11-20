{-# LANGUAGE InstanceSigs #-}

import Control.Monad

data W a = W a deriving Show

re :: a -> W a
re = W

fm :: (a -> b) -> W a -> W b
fm g (W x) = W $ g x

bi :: (a -> W b) -> W a -> W b
bi g (W x) = g x

-- ex. 1
g :: Int -> W Int -> W Int
g n = bi (W . (+n))

-- ex. 2
h :: W Int -> W Int -> W Int
h m1 m2 = bi (\x -> bi (\y -> re $ x + y) m2) m1

-- ex. 3
--      return a >>= f
--  =   W a >>= f = f a
--
--  m >>= return
--  = (W x) >>= return 
--  = W x
--  = m
--
--  (m >>= f) >>= g
--  = ((W x) >>= f) >>= g
--  = (f x) >>= g
--  = W x' >>= g
--  = g x'
--
--      m >>= (\x -> f x >>= g)
--  =   (W x) >>= (\x -> f x >>= g)
--  =   f x >>= g
--  =   W x' >>= g
--  =   g x'

-- ex. 4
jo :: W (W a) -> W a
jo = bi id
