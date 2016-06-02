{-# OPTIONS_GHC -Wall #-}

module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = let pairs = zip xs ys
                     in length $ filter (uncurry (==)) pairs

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors x = map (\y -> length $ filter (==y) x) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = let tuples = zip (countColors xs) (countColors ys)
                in sum $ map fst $ filter ((>0) . snd) tuples

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = let exact = exactMatches xs ys
                    nonExact = matches xs ys - exact
                in Move ys exact nonExact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move xs ex nonex) ys = let (Move _ ex' nonex') = getMove ys xs
                                     in ex == ex' && nonex == nonex'

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes mov codes = let isConsistentMove = isConsistent mov
                        in filter isConsistentMove codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = concatMap permutations $ filter ((n==) . length) (subsequences colors)

-- Exercise 7 -----------------------------------------

isNExactMatches :: Move -> Int -> Bool
isNExactMatches (Move _ ex _) n = ex == n

solve :: Code -> [Move]
solve code = let code_length = length code
                 isAnswer = flip isNExactMatches code_length
             in filter isAnswer . map (getMove code) $ allCodes code_length

-- Bonus ----------------------------------------------

-- fiveGuess :: Code -> [Move]
-- fiveGuess = undefined
