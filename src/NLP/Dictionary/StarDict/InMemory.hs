{-|
  Module:      NLP.Dictionary.StarDict.InMemory
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Tools for StarDict dictionaries.
  To load a dictionary you should call 'mkDictionary' with path to .ifo file
  and render function.
  Dictionary will be loaded into memory at the first access operation.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module NLP.Dictionary.StarDict.InMemory (
    StarDict (..)
  , Index
  , mkDictionary
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict.Common (IfoFile(..), IfoFilePath, readIfoFile, getIndexNumber)
import NLP.Dictionary.StarDict.Common (readIndexFile, checkDataFile, Renderer)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map


-- | Representation of an .idx file.
type Index = Map Text (Int, Int)

-- | Representation of dictionary.
data StarDict = StarDict {
    sdIfoFile    :: IfoFile
  , sdIndex      :: Index
  , sdData       :: ByteString
  , sdRender     :: Renderer
  } deriving Generic

instance NFData StarDict


-- | Create dictionary.
mkDictionary :: (MonadThrow m, MonadIO m) => IfoFilePath -> Renderer -> m StarDict
mkDictionary ifoPath sdRender = do
  sdIfoFile  <- readIfoFile   ifoPath
  sdIndex    <- Map.fromList <$> readIndexFile ifoPath (getIndexNumber . ifoIdxOffsetBits $ sdIfoFile)
  sdData     <- checkDataFile ifoPath >>= liftIO . BS.readFile
  return StarDict {..}

instance Dictionary StarDict where
  getEntries str (StarDict {..}) = return extractEntries where

    extractEntries :: [Text]
    extractEntries = map extractEntry . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: (Int, Int) -> Text
    extractEntry (offset, size) = decodeUtf8
                                . BS.take (fromIntegral size)
                                . BS.drop (fromIntegral offset) $ sdData
