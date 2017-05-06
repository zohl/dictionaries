{-|
  Module:      NLP.Dictionary.StarDict.Regular
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Tools for StarDict dictionaries.
  Every call of 'getEntries' will perform file reading operation to
  retrieve requested data. For faster access see
  'NLP.Dictionary.StarDict.InMemory'.
-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module NLP.Dictionary.StarDict.Regular (tag) where

import Prelude hiding (takeWhile)
import Data.Binary.Get (runGet)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Text.Lazy (Text)
import System.IO (Handle, IOMode(..), SeekMode(..), withFile, hSeek)
import NLP.Dictionary (Dictionary(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import NLP.Dictionary.StarDict.Common (StarDict(..), IfoFile(..), readIfoFile, readIndexFile, getIndexNumber, checkDataFile, mkDataParser, Renderer, IfoFilePath)
import Data.Tagged (Tagged(..), untag)


type Index = Map Text (Int, Int)

data Implementation = Implementation {
    sdIfoFile    :: IfoFile
  , sdIndex      :: Index
  , sdDataPath   :: FilePath
  , sdRender     :: Renderer
  } deriving Generic

instance NFData Implementation

instance Dictionary Implementation where
  getEntries str (Implementation {..}) = withFile sdDataPath ReadMode extractEntries where

    extractEntries :: Handle -> IO [Text]
    extractEntries h = mapM (extractEntry h) . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: Handle -> (Int, Int) -> IO Text
    extractEntry h (offset, size) = do
      hSeek h AbsoluteSeek (fromIntegral offset)
      T.concat . map sdRender . runGet (mkDataParser . ifoSameTypeSequence $ sdIfoFile) <$> BS.hGet h size

instance StarDict Implementation where
  getIfoFile = sdIfoFile

  mkDictionary taggedIfoPath sdRender = do
    let ifoPath = untag taggedIfoPath
    sdIfoFile  <- readIfoFile ifoPath
    sdIndex    <- Map.fromList <$> readIndexFile ifoPath (getIndexNumber . ifoIdxOffsetBits $ sdIfoFile)
    sdDataPath <- checkDataFile ifoPath
    return Implementation {..}

-- | Tag for ifoPath to distinct dictionary type.
tag :: IfoFilePath -> Tagged Implementation IfoFilePath
tag = Tagged
