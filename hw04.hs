module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2

instance (Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = p1 == p2

exercise2 :: IO ()
exercise2 = do
    print $ P [1, 2, 3] == P [1, 2, 3]
    print $ P [1, 2] /= P [1, 2, 3]

-- Exercise 3

instance (Show a, Eq a, Num a) => Show (Poly a) where
    show (P p) = polyPrint p
        where polyPrint = intercalate " + " . filter (not . null) . map prettify . reverse . zip [0..]
              prettify (nth, variable)
                | variable == 0 = ""
                | nth == 0 = show variable
                | otherwise = suffix variable ++ postfix nth
                    where suffix v
                            | v == 1 = ""
                            | v == -1 = "-"
                            | otherwise = show v
                          postfix v
                            | v > 1 = "x^" ++ show v
                            | otherwise = "x"

exercise3 :: IO ()
exercise3 = do
    print $ show (P [1, 0, 0, 2]) == "2x^3 + 1"
    print $ show (P [0, -1, 2]) == "2x^2 + -x"

-- Exercise 4

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = let len = max (length p1) (length p2)
                         p1' = p1 ++ repeat 0
                         p2' = p2 ++ repeat 0
                     in P $ take len $ zipWith (+) p1' p2'

instance (Num a) => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P $ map negate p
    fromInteger = P . (:[]) . fromInteger
    --
    abs = undefined
    signum = undefined

exercise4 :: IO ()
exercise4 = do
    print $ P [5, 0, 1] + P [1, 1, 2] == P [6, 1, 3]
    print $ P [1, 0, 1] + P [1, 1] == P [2, 1, 1]

-- Exercise 5

times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = let (less, big) | length p1 > length p2 = (p1, p2)
                                      | otherwise = (p2, p1)
                          indexedMult = zip [0..] big
                          timesLst = map (P . (\(idx, val) -> map (val *) (replicate idx 0 ++ less))) indexedMult
                      in sum timesLst

exercise5 :: IO ()
exercise5 = print $ P [1, 1, 1] * P [2, 2] == P [2, 4, 4, 2]

-- Exercise 6

exercise6 :: IO ()
exercise6 = print $ negate (P [1, 2, 3]) == P [-1, -2, -3]

-- Exercise 7

applyP :: Num a => Poly a -> a -> a
applyP (P p) x = sum $ map (\(idx, v) -> v * x ^ idx) $ zip [0..] p

exercise7 :: IO ()
exercise7 = do
    print $ applyP (x^2 + 2*x + 1) 1 == 4
    print $ applyP (x^2 + 2*x + 1) 2 == 9

-- Exercise 8

class Num a => Differentiable a where
    deriv :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 p = deriv p
    nderiv n p = deriv $ nderiv (n-1) p

-- Exercise 9

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P p) = P $ tail $ zipWith (*) [0..] p

exercise9 :: IO ()
exercise9 = print $ deriv (x^2 + 3*x + 5) == 2*x + 3
