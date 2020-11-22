myFix :: (a -> a) -> a
myFix f =
    let x = f x
     in x

-- fix id =
--
-- let x = id x
--  in x
--
-- = let x = id x
--      in id x
-- = let x = id x
--      in id (id x)
--  = let x = id x
--      in id (id (id x))
--
-- or: (not sure how the evaluation actually proceeds)
-- let x = id x
--  in x
--  = let x = id (id x)
--   in x
--  = let x = id (id (id x))
--   in x
--   ...
--   = let x = (id . id . id ... id) x
--      in x

last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs

-- myFix :: ((b -> c) -> (b -> c)) -> b -> c
last'' :: [a] -> Maybe a 
last'' = myFix $ \f xs ->
    case xs of
      [] -> Nothing
      [x] -> Just x
      (x:xs) -> f xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' f = myFix $ \recurse xs ->
    case xs of
      [] -> []
      (x:xs) -> f x : recurse xs

-- myFix :: (a -> a) -> a
-- myFix f =
--     let x = f x
--      in x

--      last'' [1..3]
-- =    fix (\f xs -> ...) [1..3]
-- =    (\f xs -> ...) (\f xs -> ...) [1..3]
-- =    (\xs ->
--           case xs of
--             [] -> Nothing
--             [x] -> Just x
--             (x:xs) -> (\f xs -> ...) xs
--       ) [1..3]
--  =   case [1..3] of
--  ...
--  = (\f xs -> ...) [2..3]
--  = ...
--  this seems familiar.

-- monadic voodoo. This is fix, but with b = a and c = m b. recall:
-- myFix :: ((b -> c) -> (b -> c)) -> b -> c
fixMonad :: Monad m => ((a -> m b) -> (a -> m b)) -> a -> m b
fixMonad = myFix

printUntilZero :: Int -> IO Int
printUntilZero = myFix $ \f x ->
    if x >= 0
       then do
           print x
           f (x - 1)
        else pure x

-- myFix :: (a -> a) -> a
-- myFix f =
--     let x = f x
--      in x

-- so far: recursive functions
-- now: recursive structures
-- myFix :: (a -> a) -> a
-- listFix :: ([a] -> [a]) -> [a]
infList = myFix (\xs -> 1 : ((+1) <$> xs))
-- infList = myFix (\xs -> 1 : ((+1) <$> xs))
--          = (\xs -> 1 : ((+1) <$> xs)) x
--          = 1 : ((+1) <$> x)
--          = 1 : ((+1) <$> ((\xs -> 1 : ((+1) <$> xs)) x))
--          = 1 : ((+1) 1) : (+1) <$> ((+1) <$> x)
--          = 1 : 2 : (+1) <$> ((+1) <$> x)
--          = ...

fibonacciFix :: [Integer]
fibonacciFix = myFix $ \xs -> 0 : 1 : zipWith (+) xs (tail xs)

-- more magic: ~ means lazy pattern match o_O
-- myFix :: (a -> a) -> a
-- myFix :: (([Int],Int,Int) -> ([Int],Int,Int)) -> ([Int],Int,Int)
lazyFix = myFix $ \(~(a,b,c)) -> ([1,c-5], head a + 2, b * 4)

lazyFix' = let   a = [1, c-5]
                 b = head a + 2
                 c = b * 4
            in (a,b,c)
            -- ^ has a cyclic dependency, which is broken by the list in a. uh oh.
