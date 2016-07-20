{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

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
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s) ++ ", ..."

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
sInterleave (Cons x xs) (Cons y ys) = Cons x ( Cons y ( sInterleave xs ys ) )

sTake :: Int -> Stream a -> [a]
sTake n s = take n (streamToList s)
