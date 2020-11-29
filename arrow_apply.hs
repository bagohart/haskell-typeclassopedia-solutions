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

-- instance for (->)
newtype Func a b = Func { runFunc :: a -> b }

instance Category Func where 
    id :: Func a a
    id = Func Prelude.id
    (.) (Func g) (Func h) = Func (g Prelude.. h)

instance Arrow Func where 
    arr :: (b -> c) -> Func b c
    arr g = Func g
    first :: Func b c -> Func (b,d) (c,d)
    first (Func g) = Func $ \(b,d) -> (g b,d)

instance ArrowApply Func where 
    app :: Func (Func b c,b) c
    app = Func $ \((Func g),b) -> g b

app2' :: Func b (Func (Func b c) c)
app2' = Func $ \b -> Func $ \(Func g) -> g b

-- exercise: now use app, but not specific properties of Func to implement this.
app2'' :: ArrowApply ar => ar b (ar (ar b c) c)
app2'' = arr $ \b -> ((arr $ \g -> (g,b)) >>> app)
