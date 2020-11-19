{-# LANGUAGE InstanceSigs #-}

import Data.Functor

iChangeTheThings :: Maybe Int
iChangeTheThings = 5 <$ Just "lol"

-- Exercises block 1
-- 1.
data Either' a b = Left' a | Right' b deriving (Show, Eq)

instance Functor (Either' e) where 
    fmap :: (a -> b) -> (Either' e a) -> (Either' e b)
    fmap _ (Left' x) = Left' x
    fmap g (Right' x) = Right' $ g x

    (<$) :: a -> Either' e b -> Either' e a
    (<$) _ (Left' x) = Left' x
    (<$) a (Right' x) = Right' a

data Reader' e a = Reader' (e -> a)

instance Functor (Reader' e) where 
    fmap :: (a -> b) -> Reader' e a -> Reader' e b
    fmap g (Reader' r) = Reader' $ \e -> g (r e)

    (<$) :: a -> Reader' e b -> Reader' e a
    (<$) a _ = Reader' $ const a
    -- i.e.
    -- (<$) a _ = Reader' $ \e -> a

-- 2.
data Pair a = Pair a a deriving (Show, Eq)

-- has only one type parameter, so both values are replaced
instance Functor Pair where 
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap g (Pair x y) = Pair (g x) (g y)

    (<$) :: a -> Pair b -> Pair a
    (<$) a _ = Pair a a

data Pair' a b = Pair' (a,b) deriving (Show,Eq)

-- has two type parameters, so both values are replaced
instance Functor (Pair' e) where 
    fmap :: (a -> b) -> Pair' e a -> Pair' e b
    fmap g (Pair' (x,y)) = Pair' (x, (g y))

    (<$) :: a -> Pair' e b -> Pair' e a
    (<$) a (Pair' (x,_)) = Pair' (x,a)

-- 3.
data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where 
    fmap :: (a -> b) -> ITree a -> ITree b
    fmap gab (Leaf gia) = Leaf $ (gab . gia)
    -- fmap gab (Node arr) = Node $ map (fmap gab) arr -- or just:
    fmap gab (Node arr) = Node $ (fmap . fmap) gab arr

    (<$) :: a -> ITree b -> ITree a
    (<$) a (Leaf gia) = Leaf $ const a
    (<$) a (Node arr) = Node $ map (a<$) arr

-- 4.
-- Looks like Reader, but the Functor needs to map the a which is now the "read" variable
data NotAReader e a = NotAReader (a -> e)

-- instance Functor (NotAReader e) where 
--     fmap g (NotAReader gae) = ??? -- needs to become (b -> e) but we only have (a -> e) and (a -> b)
--     this seems like it shouldn't work

-- 5.
-- Functors can be composed:
newtype Compose f g a = Compose (f (g a)) deriving (Eq,Show) -- not sure why I need the (), but I do

instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap g (Compose fga) = Compose $ (fmap . fmap) g fga

    (<$) :: a -> Compose f g b -> Compose f g a
    (<$) x (Compose fga) = Compose $ fmap (x<$) fga

----
    
data EvilList a = EvilList { getEvilList :: [a] } deriving (Eq,Show)

-- Evil Functor instance
instance Functor EvilList where
  fmap :: (a -> b) -> EvilList a -> EvilList b
  fmap _ (EvilList []) = EvilList []
  fmap g (EvilList (x:xs)) = EvilList $ g x : g x : getEvilList (fmap g (EvilList xs))
  --             ^^^^^^^^^^^^
  --             changes the structure of the list. it has now more elements.
  --             In particular, fmap id [1] = [1,1] and
  --             fmap id [x1..xn] = [x1,x1..xn,xn]

-- Exercises block 2
-- 1.
-- first law: fmap id = id
-- second law: fmap (g.h) = fmap g . fmap h
-- Find a Functor instance where the second law holds, but the first does not:
data BogusFunctor a = BogusIdentity a | BogusNothing

instance Functor BogusFunctor where 
    fmap g _ = BogusNothing

-- now, fmap (g.h) = fmap g . fmap h = const BogusNothing
-- but fmap id (BogusIdentity x) = BogusNothing != BogusIdentity x

-- The following example shows that with bottom trickery we can construct a Functor instance that
-- satisfies the first law, but not the second law:
data MoreBogus a = MoreBogus a deriving Show

instance Functor MoreBogus where 
    fmap g (MoreBogus x) = g `seq` MoreBogus (g x)
-- because fmap (const 5 . undefined) (MoreBogus 1) != fmap (const 5) . fmap undefined $ MoreBogus 1
-- lol.


-- 2. The evil list instance violates both laws:
-- fmap id [1] = [1,1] != id [1]
-- and
-- fmap (+1).(subtract 1) [1] = [1,1]
-- fmap (+1) . fmap (subtract 1) = [1,1,1,1]
evilListExample1 = fmap id (EvilList [1])
evilListExample2 = fmap ((+1) . (subtract 1)) (EvilList [1::Int])
evilListExample2' = (fmap ((+1)) . (fmap (subtract 1))) (EvilList [1::Int])

-- more utility things
util1 = (+1) <$> [1,2,3]
util2 = [1,2,3] $> 1337
util3 = void [1,2,3]
