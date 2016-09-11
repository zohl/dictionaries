module NLP.Dictionary (
    Dictionary(..)
  ) where

import Data.ByteString.Lazy (ByteString)

class Dictionary a where
  getEntries :: ByteString -> a -> IO [ByteString]
