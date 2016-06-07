module Hw05 where

import Data.ByteString (ByteString)
import Data.List
import qualified Data.ByteString as BS

-- Exercise 1

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret oldf newf = do
    old <- BS.readFile oldf
    new <- BS.readFile newf
    return $ BS.pack $ BS.unpack old \\ BS.unpack new

exercise1 :: IO ByteString
exercise1 = getSecret "clues/dog.jpg" "clues/dog-original.jpg"

-- Exercise 2

-- decryptWithKey :: ByteString -> FilePath -> IO ()
