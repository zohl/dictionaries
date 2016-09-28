{-|
  Module:      NLP.Dictionary.StarDict.InMemory
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  In-memory version of 'NLP.Dictionary.StarDict'.
-}

{-# LANGUAGE RecordWildCards #-}

module NLP.Dictionary.StarDict.InMemory (
    StarDict (..)
  , mkDictionary
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.Binary.Get (Get, getWord32be)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (maybeToList)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (IfoFile(..), IfoFilePath, readIfoFile, indexNumberParser)
import NLP.Dictionary.StarDict (Index, readIndexFile, checkDataFile, DataEntry(..), Renderer)
import NLP.Dictionary.StarDict (mkDataParser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

-- | Representation of dictionary.
data StarDict = StarDict {
    sdIfoFile    :: IfoFile
  , sdIndex      :: Index
  , sdData       :: ByteString
  , sdDataParser :: Get [DataEntry]
  , sdRender     :: Renderer
  }

-- | Create dictionary.
mkDictionary :: (MonadThrow m, MonadIO m) => IfoFilePath -> Renderer -> m StarDict
mkDictionary ifoPath sdRender = do
  sdIfoFile  <- readIfoFile   ifoPath
  sdIndex    <- readIndexFile ifoPath (indexNumberParser sdIfoFile)
  sdData     <- checkDataFile ifoPath >>= liftIO . BS.readFile
  let sdDataParser = mkDataParser (ifoSameTypeSequence sdIfoFile)
  return StarDict {..}

instance Dictionary StarDict where
  getEntries str (StarDict {..}) = return extractEntries where

    extractEntries :: [Text]
    extractEntries = map extractEntry . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: (Int, Int) -> Text
    extractEntry (offset, size) = decodeUtf8
                                . BS.take (fromIntegral size)
                                . BS.drop (fromIntegral offset) $ sdData
