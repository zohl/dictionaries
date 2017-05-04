{-|
  Module:      NLP.Dictionary.StarDict
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Tools for StarDict dictionaries.
  To load a dictionary you should call 'mkDictionary' with path to .ifo file
  and render function.
  Every call of getEntry will perform file reading operation to retrieve
  requested data. For in-memory version see 'NLP.Dictionary.StarDict.InMemory'.
-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module NLP.Dictionary.StarDict (
    StarDict (..)
  , mkDictionary
  , Index
  ) where

import Prelude hiding (takeWhile)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
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
import NLP.Dictionary.StarDict.Common (IfoFilePath, IfoFile(..), readIfoFile, readIndexFile, getIndexNumber, checkDataFile, mkDataParser, Renderer)


-- | Representation of an .idx file.
type Index = Map Text (Int, Int)

-- | Representation of the dictionary.
data StarDict = StarDict {
    sdIfoFile    :: IfoFile
  , sdIndex      :: Index
  , sdDataPath   :: FilePath
  , sdRender     :: Renderer
  } deriving Generic

instance NFData StarDict

-- | Create dictionary.
mkDictionary :: (MonadThrow m, MonadIO m) => IfoFilePath -> Renderer -> m StarDict
mkDictionary ifoPath sdRender = do
  sdIfoFile    <- readIfoFile   ifoPath
  sdIndex      <- Map.fromList <$> readIndexFile ifoPath (getIndexNumber . ifoIdxOffsetBits $ sdIfoFile)
  sdDataPath   <- checkDataFile ifoPath
  return StarDict {..}

instance Dictionary StarDict where
  getEntries str (StarDict {..}) = withFile sdDataPath ReadMode extractEntries where

    extractEntries :: Handle -> IO [Text]
    extractEntries h = mapM (extractEntry h) . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: Handle -> (Int, Int) -> IO Text
    extractEntry h (offset, size) = do
      hSeek h AbsoluteSeek (fromIntegral offset)
      T.concat . map sdRender . runGet (mkDataParser . ifoSameTypeSequence $ sdIfoFile) <$> BS.hGet h size
