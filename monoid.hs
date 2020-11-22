import Data.Monoid
import Data.List

x1 = First (Just 1) <> First Nothing <> First (Just 3)
x2 = Last (Just 1) <> Last Nothing <> Last (Just 3)

x3 :: Int
x3 = appEndo ((Endo (+1)) <> (Endo (subtract 1))) $ 5

x4 :: (Sum Int, Product Int)
x4 = (Sum 5, Product 6) <> (Sum 7, Product 8)

x5 :: Product Int
x5 = ((+1) <> (+5)) $ 1

x6 :: [Int]
x6 = sortBy (compare) [1..5]

x6' :: [String]
x6' = sortBy ((\f s -> if 'l' `elem` f then LT else EQ) <> compare) ["nope","lol"]

x6'' :: [String]
x6'' = sortBy ((\f s -> compare (length f) (length s)) <> compare) ["nope","lol","z"]
-- ^ this is pretty cool
