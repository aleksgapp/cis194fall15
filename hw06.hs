{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List

-- Exercise 1 --------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 --------------------------------

fibs2 :: [Integer]
fibs2 = 1:1: zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 --------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show ( Stream a ) where
    show s = "[" ++ intercalate ", " (map show $ take 20 $ streamToList s) ++ ", ..."

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

-- Exercise 4 --------------------------------

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

-- Exercise 5 --------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons v (sIterate f v)
    where v = f x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x $ sInterleave y xs

sTake :: Int -> Stream a -> [a]
sTake n s = take n (streamToList s)

-- Exercise 6 --------------------------------

nats :: Stream Integer
nats = Cons 0 (fmap (+1) nats)

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)

-- Exercise 7 --------------------------------

rand :: Int -> Stream Int
rand seed = let seq' = Cons (getNext seed) (fmap getNext seq')
            in seq'
    where getNext prev = (1103515245 * prev + 12345) `mod` 2147483648

-- Exercise 8 --------------------------------

{- Total Memory in use: 256 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 --------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x : xs) = Just $ minMax' x x xs
    where minMax' aMin aMax [] = (aMin, aMax)
          minMax' aMin aMax (y : ys)
            | y < aMin = minMax' y aMax ys
            | y > aMax = minMax' aMin y ys
            | otherwise = minMax' aMin aMax ys

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 -------------------------------

data Matrix = M { tLeft :: Integer
                   , tRight :: Integer
                   , bLeft :: Integer
                   , bRight :: Integer
                   }

instance Num Matrix where
      m1 * m2 = M { tLeft = tLeft m1 * tLeft m2 + tRight m1 * bLeft m2
                  , tRight = tLeft m1 * tRight m2 + tRight m1 * bRight m2
                  , bLeft = bLeft m1 * tLeft m2 + bRight m1 * bLeft m2
                  , bRight = bLeft m1 * tRight m2 + bRight m1 * bRight m2
                  }

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = tRight $ initial ^ (n+1)
    where initial = M { tLeft = 1, tRight = 1, bLeft = 1, bRight = 0 }
