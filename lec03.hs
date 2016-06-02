module LEC3 where

import Data.Char ( toUpper )

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
    deriving Show

shoe :: Thing
shoe = Shoe

listOThings :: [Thing]
listOThings = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

data FailableDouble = Failure
                    | OK Double
    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- Store a person's name, age and favorite Thing
data Person = Person String Int Thing
    deriving Show

richard :: Person
richard = Person "Richard" 32 Ship

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ Ship) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)    = n ++ ", your favorite thing is lame."

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                    Failure -> 0
                    OK d    -> d

data LogMessage = LogMessage Int String
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
data MaybeInt = ValidInt Int
              | InvalidInt

exampleA :: Maybe Int -> Int
exampleA (Just n) = n
exampleA Nothing = -1

exampleB :: LogMessage -> Maybe String
exampleB (LogMessage severity s) | severity >= 50 = Just s
exampleB _                                        = Nothing

data List t = Empty | Cons t (List t)

lst1 :: List Int
lst1 = Cons 3 ( Cons 5 ( Cons 2 Empty ) )

lst2 :: List Char
lst2 = Cons 'x' ( Cons 'y' ( Cons 'z' Empty ) )

lst3 :: List Bool
lst3 = Cons True ( Cons False Empty )

intListProd :: List Int -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
    deriving Show

tree :: Tree
tree = Node ( Leaf 'x' ) 1 ( Node ( Leaf 'y' ) 2 ( Leaf 'z' ) )
