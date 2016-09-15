module NLP.Dictionary (
    Dictionary(..)
  ) where

import Data.Text.Lazy (Text)

class Dictionary a where
  getEntries :: Text -> a -> IO [Text]
