{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Arrow
import Control.Monad
import Data.Monoid

-- class Arrow arr => ArrowChoice arr where 
--  left :: (b `arr` c) -> (Either b d `arr` Either c d) -- builds a new ArrowChoice where the left value is the old Arrow
--  right :: (b `arr` c) -> (Either d b `arr` Either d c) -- like left, but right :)
--  (+++) :: (b `arr` c) -> (b' `arr` c') -> (Either b b' `arr` Either c c') -- chooses the 'right' Arrow ? o_O
--  (|||) :: (b `arr` d) -> (c `arr` d) -> (Either b c `arr` d) -- like +++, but the result type is the same, so...
--  This looks very like Arrow, but on sum types instead of product types

-- todo:
-- left and +++ is minimal, so define right and ||| in their terms.
-- define instances for (->) and Kleisli
