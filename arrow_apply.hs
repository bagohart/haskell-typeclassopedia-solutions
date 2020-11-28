{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Arrow
import Control.Monad
import Data.Monoid

-- class Arrow arr -> ArrowApply arr where 
--  app :: (b `arr` c, b) `arr` c
--  ^ this looks weird. apparently, the arrow in the first argument is applied to the value in the first argument. hm.
--  todo:
--  app2
--  various instances to show equivalence between ArrowApply and Monad
