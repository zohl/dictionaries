module NLP.Dictionary (
    Dictionary(..)
  ) where

import Data.Text.Lazy (Text)

-- | Core type class that provides interface to dictionaries.
class Dictionary a where
  getEntries :: Text -> a -> IO [Text]
