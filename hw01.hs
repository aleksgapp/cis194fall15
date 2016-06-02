{-# OPTIONS_GHC -Wall #-}
module HW01 where

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

unDigit :: [Integer] -> Integer
unDigit [] = 0
unDigit (x:xs) = x * 10 ^ length xs + unDigit xs

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n
    | n < 1 = 0
    | otherwise = last $ digits n

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

exercise1 :: IO ()
exercise1 = do
    print $ lastDigit 123 == 3
    print $ lastDigit 0 == 0
    print $ dropLastDigit 123 == 12
    print $ dropLastDigit 5 == 0

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = reverse . digits

exercise2 :: IO ()
exercise2 = do
    print $ toRevDigits 1234 == [4,3,2,1]
    print $ null $ toRevDigits 0
    print $ null $ toRevDigits (-17)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

exercise3 :: IO ()
exercise3 = do
    print $ doubleEveryOther [4, 9, 5, 5] == [4, 18, 5, 10]
    print $ doubleEveryOther [0, 0] == [0, 0]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . digits)

exercise4 :: IO ()
exercise4 = print $ sumDigits [10, 5, 18, 4] == 19

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = let f = sumDigits . doubleEveryOther . reverse . digits
             val = f x
         in (val `mod` 10) == 0

exercise5 :: IO ()
exercise5 = do
    print $ luhn 5594589764218858
    print $ not $ luhn 1234567898765432

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

exercise6 :: IO ()
exercise6 = print $ hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
