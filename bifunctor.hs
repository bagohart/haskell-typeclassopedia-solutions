import Data.Bifunctor
import Data.Functor.Const

tuple :: (Int,Int)
tuple = (1,2)

b0 = bimap (+1) (+1) tuple
b1 = first (+1) tuple
b2 = second (+1) tuple

ei1 = Left 1
ei2 = Right 2

e0 = bimap (+1) (+1) ei1
e1 = first (+1) ei1
e2 = second (+1) ei1

e0' = bimap (+1) (+1) ei2
e1' = first (+1) ei2
e2' = second (+1) ei2

constor = Const 1

c0 = bimap (+1) (+1) constor
c1 = first (+1) constor
c2 = second (+1) constor
