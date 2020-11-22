import Data.Semigroup
import Data.List.NonEmpty

-- class Semigroup a where 
-- (<>) :: a -> a -> a
-- sconcat :: NonEmpty a -> a
-- stimes :: Integral b => b -> a -> a

x1 = All True <> All False

x2 = sconcat $ fromList [Any True, Any False]

x3 = stimes 5 $ Any True

x4 = stimes 5 $ Sum 3
