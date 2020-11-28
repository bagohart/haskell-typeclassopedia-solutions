{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Arrow
import Control.Monad
import Data.Monoid

-- class Arrow a => ArrowLoop a where 
--  loop :: a (b, d) (c, d) -> a b c
--
--  trace :: ((b,d) -> (c,d)) -> b -> c
--  trace f b = let (c,d) = f (b,d) in c
-- this looks like magic, probably like MonadFix o_O
