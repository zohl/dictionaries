{-# LANGUAGE RecordWildCards #-}

module NLP.Dictionary.StarDict.InMemory (
    StarDict (..)
  , mkDictionary
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.Binary.Get (getWord32be)
import Data.Maybe (maybeToList)
import Data.ByteString.Lazy (ByteString)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (IfoFile(..), IfoFilePath, readIfoFile, indexNumberParser)
import NLP.Dictionary.StarDict (Index, readIndexFile, checkDataFile)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

data StarDict = StarDict {
    sdIfoFile :: IfoFile
  , sdIndex   :: Index
  , sdData    :: ByteString
  } deriving (Eq, Show)

mkDictionary :: (MonadThrow m, MonadIO m) => IfoFilePath -> m StarDict
mkDictionary ifoPath = do
  sdIfoFile  <- readIfoFile   ifoPath
  sdIndex    <- readIndexFile ifoPath (indexNumberParser sdIfoFile)
  sdData     <- checkDataFile ifoPath >>= liftIO . BS.readFile
  return StarDict {..}

instance Dictionary StarDict where
  getEntries str (StarDict {..}) = return extractEntries where

    extractEntries :: [ByteString]
    extractEntries = map extractEntry . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: (Int, Int) -> ByteString
    extractEntry (offset, size) = BS.take (fromIntegral size)
                                . BS.drop (fromIntegral offset) $ sdData
