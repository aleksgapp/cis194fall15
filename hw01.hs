{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

unDigit :: [Integer] -> Integer
unDigit [] = 0
unDigit (x:xs) = x * 10 ^ length xs + unDigit xs

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n
    | n < 1 = 0
    | otherwise = last $ digits n

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = reverse . digits

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . digits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = let f = sumDigits . doubleEveryOther . reverse . digits
             val = f x
         in (val `mod` 10) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
