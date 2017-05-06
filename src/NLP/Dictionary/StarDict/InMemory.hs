{-|
  Module:      NLP.Dictionary.StarDict.InMemory
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Implementation of an in-memory dictionary.
  All the entries will be loaded in the beginning into RAM, thus
  allowing faster access for the next queries.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module NLP.Dictionary.StarDict.InMemory (tag) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Tagged (Tagged(..), untag)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict.Common (IfoFile(..), IfoFilePath, readIfoFile, getIndexNumber, StarDict(..))
import NLP.Dictionary.StarDict.Common (readIndexFile, checkDataFile, Renderer)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map


type Index = Map Text (Int, Int)

data Implementation = Implementation {
    sdIfoFile    :: IfoFile
  , sdIndex      :: Index
  , sdData       :: ByteString
  , sdRender     :: Renderer
  } deriving Generic

instance NFData Implementation

instance Dictionary Implementation where
  getEntries str (Implementation {..}) = return extractEntries where

    extractEntries :: [Text]
    extractEntries = map extractEntry . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: (Int, Int) -> Text
    extractEntry (offset, size) = decodeUtf8
                                . BS.take (fromIntegral size)
                                . BS.drop (fromIntegral offset) $ sdData

instance StarDict Implementation where
  getIfoFile = sdIfoFile

  mkDictionary taggedIfoPath sdRender = do
    let ifoPath = untag taggedIfoPath
    sdIfoFile  <- readIfoFile ifoPath
    sdIndex    <- Map.fromList <$> readIndexFile ifoPath (getIndexNumber . ifoIdxOffsetBits $ sdIfoFile)
    sdData     <- checkDataFile ifoPath >>= liftIO . BS.readFile
    return Implementation {..}

-- | Tag for ifoPath to distinct dictionary type.
tag :: IfoFilePath -> Tagged Implementation IfoFilePath
tag = Tagged
