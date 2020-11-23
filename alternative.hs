import Control.Applicative
import Control.Monad -- for guard, which has an Alternative constraint o_O
import Data.Foldable

-- class Applicative f => Alternative f where 
--  empty :: f a
--  (<|>) :: f a -> f a -> f a
--
--  some :: f a -> f [a]
--  many :: f a -> f [a]
--  ^ intuition: run v until it fails, collect its results into a list.
--      some needs to succeed at least once, many not.
--      this does not always make sense o_O
--
--  MonadPlus is like Alternative. Mostly for historical reasons. This is a mess.
--  Almost no one uses ArrowPlus.
--
--  instance Alternative Maybe where 
--      empty = Nothing
--      (<|>) Nothing y = y
--      (<|>) (Just x) _ = Just x
--
--  many Nothing = some Nothing <|> pure []
--  = ((:) <$> Nothing <*> many Nothing) <|> pure []
--  = (Nothing <*> many Nothing) <|> pure []
--  = Nothing <|> pure []
--  = pure []
--  = Just []
--
--  some Nothing = (:) <$> Nothing <*> many Nothing
--  = Nothing <*> many Nothing
--  = Nothing
--
--  many (Just 1) = some (Just 1) <|> pure []
--  = ((:) <$> Just 1 <*> many Just 1) <|> pure []
--  = (Just (1:) <*> many (Just 1)) <|> pure []
--  = (Just (1:) <*> (some (Just 1) <|> pure [])) <|> pure []
--  = (Just (1:) <*> ((:) <$> Just 1 <*> many (Just 1))) <|> pure []
--  = (Just (1:) <*> (Just (1:) <*> many (Just 1))) <|> pure []
--  = ... this won't terminate. It just adds 1's forever.
--  It doesn't even do anything useful with lazy eval, because <*> needs to evaluate
--  the right element to Just before doing anything.
--
x1 :: Maybe Int
x1 = asum [Nothing, Just 1, Nothing, Just 2]

x1' :: Maybe Int
x1' = Nothing <|> Just 2 <|> Nothing <|> Just 1

-- instance Alternative [] where 
--  empty = []
--  (<|>) = (++)
--  ^ this is like Monoid for []!
--  many [1,2] = some [1,2] <|> pure []
--  = ((:) <$> [1,2] <*> many [1,2]) <|> pure []
--  = ([(1:),(2:)] <*> many [1,2]) <|> pure []
--  = ([(1:),(2:)] <*> (some [1,2] <|> pure []) <|> pure []
--  = ([(1:),(2:)] <*> ((((:) <$> [1,2] <*> many [1,2]) <|> pure []) <|> pure []) <|> pure []
--  = ... this is similar to Maybe. It tries to produce an infinite list of all kinds of things,
--  but it never gets anywhere. aka this is useless o_O
--  for []:
--  many [] = some [] <|> pure []
--  =(:) <$> [] <*> many []
--  = [] <*> many []
--  = []
--  some [] = (:) <$> [] <*> many []
--  = [] <*> many []
--  = []
--
--  this is useful only for stateful Applicative instances where the action succeeds a finite number of times and then fails.
--  Original motivating example: parsers.
--
--  and suddenly: IO. this is a bit of a hack. ...
x2 = readFile "idontexist" <|> return "The file does not really contain this content. AHAHAHA."
x3 = empty <|> putStrLn "empty was caught and then ignored"

-- laws
-- 0) monoid laws:
-- empty <|> x = x
-- x <|> empty = x
-- (x <|> y) <|> z = x <|> (y <|> z)
-- Other than that, no one agrees :) How should <|> and <*> interact?
--
-- 1) Left zero: (mostly uncontroversial)
-- empty <*> f = empty
--
-- 2) Right zero:
-- f <*> empty = empty
-- ^ not satisfied by IO, side effects cannot be back-rolled.
-- There is also Backwards, a transformer for Applicative/Alternative.
-- It performs the actions of arguments to <*> in reverse order.
-- This means, Left Zero is broken, too :)
--
-- 3) Left distribution:
-- (a <|> b) <*> c = (a <*> c) <|> (b <*> c)
-- holds for Maybe and [].
-- But not for IO and most parsers :'(
-- Counter example for IO:
-- a removes "thefile.txt"
-- b creates "thefile.txt"
-- c reads "thefile.txt"
-- ...
--
-- Parser:
-- a = parse **
-- c = parse **
-- b = parse *
-- input = ***
-- ...
--
-- 4) Right distribution:
-- a <*> (b <|> c) = (a <*> b) <|> (a <*> c)
-- Maybe: yes.
-- Lists: no. Order is different:
-- a = [(+1), (*10)]
-- b = [2]
-- c = [3]
-- a <*> (b <|> c) = [(+1), (*10)] <*> ([2] <|> [3])
-- = [(+1), (*10)] <*> [2,3]
-- = [3,4,20,30]
-- (a <*> b) <|> (a <*> c)
-- = ([(+1), (*10)] <*> [2]) <|> ([(+1), (*10)] <*> [3])
-- = [3,20] <|> [4,30]
-- = [3,20,4,30]

-- IO: no.
-- b = c = return ()
-- a = getLine >>= \a -> if a == "lol" then ioError "may need to run twice to succeed"

-- Parsers:
-- Intuitively, it should work, but for some parsers, it doesn't because
-- for some reason some don't fully implement backtracking. I don't really get why that would be correct. hm.
--
-- 5) Left Catch:
-- (pure a) <|> x = pure a
-- For lists, this means [a] ++ x == [a]. this is wrong :)
-- For IO, Maybe, parsers it means: pure should be successful.
-- then it even holds.

-- Utility functions:
-- guard :: Alternative f => Bool -> f ()
g1 :: Maybe Int
g1 = do
    x <- Just 5
    guard $ x < 3
    return 3

g2 :: Maybe Int
g2 = do
    x <- Just 5
    guard $ x > 3
    return 3

g3 :: [Int]
g3 = do
    x <- [1..5]
    guard $ x /= 4
    return $ 2 * x

g4 :: IO String
g4 = do
    input <- getLine
    guard $ length input < 10
    return input

-- optional :: Alternative f => f a -> f (Maybe a)
o3 :: IO (Maybe String)
o3 = optional g4

o1 :: Maybe (Maybe Int)
o1 = optional Nothing

o2 :: Maybe (Maybe Int)
o2 = optional $ Just 5
