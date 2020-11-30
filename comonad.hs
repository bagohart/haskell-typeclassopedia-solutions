{-# LANGUAGE InstanceSigs #-}

class Functor w => Comonad w where 
    extract :: w a -> a
    -- ^ is like return, expect it's not
    duplicate :: w a -> w (w a)
    duplicate = extend id
    -- ^ this is join, but inverse
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate

-- this looks like a list, except it doesn't end ?_?
data Stream a = Cons a (Stream a)

instance Functor Stream where 
    fmap g (Cons x xs) = Cons (g x) (fmap g xs)

instance Comonad Stream where
    extract :: Stream a -> a
    extract (Cons x _) = x

    duplicate :: Stream a -> Stream (Stream a)
    duplicate (Cons x xs) = Cons (Cons x xs) (duplicate xs)

    extend :: (Stream a -> b) -> Stream a -> Stream b
    extend g (Cons x xs) = Cons (g (Cons x xs)) (extend g xs)

stream :: Stream Int
stream = foldr Cons undefined [1..]

stream2 :: Stream Int
stream2 = (+2) <$> stream

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (Cons x xs) = x : takeS (n-1) xs

collapseStream :: Stream Int -> Int
collapseStream (Cons x (Cons y (Cons z _))) = x + y + z

x = takeS 5 $ extend collapseStream stream2
