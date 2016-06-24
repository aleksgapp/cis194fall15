module Hw05 where

import Data.ByteString.Lazy (ByteString)
import System.Environment (getArgs)
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
getCriminal xs = head $ Map.keys $ Map.filter (== m) xs
    where m = maximum $ Map.elems xs

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow tids =
    let payers = getWithAmont (> 0)
        payees = getWithAmont (< 0)
    in revertT payers payees tids
    where getWithAmont fn = Map.toList $ Map.filter fn flow

revertT :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
revertT [] _ _ = []
revertT _ [] _ = []
revertT _ _ [] = error "No more transactions!"
revertT ((payer, stolen):xs) ((payee, need):ys) (trid:ts) =
    let newAmout = stolen + need
        transaction = Transaction payer payee (negate need) trid
    in case compare newAmout 0 of
        GT -> transaction : revertT ((payer, newAmout):xs) ys ts
        EQ -> transaction : revertT xs ys ts
        LT -> error "Not enough money!"

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path transactions = BS.writeFile path (encode transactions)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "clues/dog-original.jpg"
                        "clues/dog.jpg"
                        "clues/transactions.json"
                        "clues/victims.json"
                        "clues/new-ids.json"
                        "clues/new-transactions.json"
  putStrLn crim
