module Hw05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import Data.Maybe
import Parser

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS

-- Exercise 1

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret oldf newf = do
    old <- BS.readFile oldf
    new <- BS.readFile newf
    return $ BS.pack $ filter (/= 0) $ BS.zipWith xor old new

exercise1 :: IO ByteString
exercise1 = getSecret "clues/dog-original.jpg" "clues/dog.jpg"

-- Exercise 2

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    file <- BS.readFile (path ++ ".enc")
    BS.writeFile path (BS.pack $ BS.zipWith xor (BS.cycle key) file)

exercise2 :: IO ()
exercise2 = do
    key <- exercise1
    decryptWithKey key "clues/victims.json"

-- Exercise 3

parseFile :: FromJSON a => FilePath -> IO ( Maybe a )
parseFile = (decode <$>) . BS.readFile

exercise3 :: IO ()
exercise3 = do
    f <- parseFile "clues/victims.json" :: IO ( Maybe[TId] )
    print f

-- Exercise 4

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs v t = do
    victims <- parseFile v :: IO ( Maybe [TId] )
    transactions <- parseFile t :: IO ( Maybe [Transaction] )
    return $ filter (\x -> fromMaybe False (elem (tid x) <$> victims)) <$> transactions

exercise4 :: IO ()
exercise4 = do
    v <- getBadTs "clues/victims.json" "clues/transactions.json"
    print v

-- Exercise 5
getFlow :: [Transaction] -> Map String Integer
getFlow = foldl (flip ins) Map.empty
    where ins tr = Map.insertWith (+) (from tr) (negate $ amount tr) . Map.insertWith (+) (to tr) (amount tr)


exercise5 :: IO ()
exercise5 = let ts = [ Transaction { from = "Haskell Curry"
                                   , to = "Simon Peyton Jones"
                                   , amount = 10
                                   , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"
                                   } ]
            in print $ getFlow ts == Map.fromList [ ("Haskell Curry", -10)
                                                  , ("Simon Peyton Jones", 10)
                                                  ]

-- Exercise 6

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.findMax

exercise6 :: IO ()
exercise6 = do
    v <- getBadTs "clues/victims.json" "clues/transactions.json"
    print $ Just $ (getCriminal . getFlow <$>) v
