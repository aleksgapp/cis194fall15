module Hw05 where

import Data.ByteString.Lazy (ByteString)
import Data.Bits (xor)

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
