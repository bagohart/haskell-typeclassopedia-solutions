import Control.Monad

-- exercise
-- class monadTrans t where 
--  lift :: Monad m => m a -> t m a
--  
--  ^ What is the kind of t?
--  t m a means (t m) a
--  m :k: * -> *
--  =>
--  t :k: (* -> *) -> * ->    *
--           m        a   ((t m) a)

-- distrib ::  N (M a) -> M (N a)
-- distrib = undefined

-- join' ::  M (N (M (N a))) -> M (N a)
-- join' undefined
-- Let's implement that with concrete things:

distrib :: (Traversable n, Monad m) => n (m a) -> m (n a)
distrib = sequenceA

-- join' ::  M (N (M (N a))) -> M (N a)
join' :: Maybe ([] (Maybe ([] a))) -> Maybe ([] a)
join' mnmna = mna
    where mmnna = distrib <$> mnmna
          mmna  = (fmap . fmap) join mmnna
          mna   = join mmna

t = Just [Just[1], Just [2,3]]
-- join' t = Just [1,2,3]
t' = Just [Just[1], Just [2,3], Nothing]
-- join' t' = Nothing

-- idea: transform M (N (M (N a)))
-- { use distrib on first N }
-- ~> M (M (N (N a)))
-- { use join on both monads }
-- ~> M (N (M a)) ~> N (M (M a)) ~> N (M a)
