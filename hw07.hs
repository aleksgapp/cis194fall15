{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
-- import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= \x -> return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV a b v = liftM2 (\va vb -> v // [(a, vb), (b, va)]) (v !? a) (v !? b)

exercise1 :: IO()
exercise1 = do
    print $ liftM (+1) (Just 5) == Just 6
    print $ swapV 0 2 (V.fromList [1, 2, 3]) == Just (V.fromList [3, 2, 1])
    print $ swapV 0 2 (V.fromList [1, 2]) == Nothing

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs v = mapM getEl xs
    where getEl i = v !? i

exercise2 = print $ getElts [1,3] (V.fromList [0..9]) == Just [1, 3]
