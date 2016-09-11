{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NLP.Dictionary.StarDict (
    StarDict (..)
  , mkDictionary
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative (liftA2, many)
import Control.Monad (when)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, endOfLine, skipSpace, char)
import Data.Binary.Get (Get, runGet, isEmpty, getLazyByteStringNul, getWord32be)
import Data.Char (chr)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Word
import System.Directory (doesFileExist)
import System.FilePath.Posix (dropExtension, (-<.>))
import System.IO (Handle, IOMode(..), SeekMode(..), withFile, hSeek)
import Data.ByteString.Lazy (ByteString)
import NLP.Dictionary (Dictionary(..))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

data IfoFile = IfoFile {
    ifoMagicData :: ByteString
  , ifoVersion   :: ByteString
  , ifoData      :: Map ByteString ByteString
  } deriving (Eq, Show)

readIfoFile :: FilePath -> IO IfoFile
readIfoFile ifoPath = BS.readFile ifoPath >>= parseContents where
  parseContents contents = case (parse ifoFile contents) of
    (Fail _ _ _) -> error "TODO"
    (Done _ r)   -> return r

  ifoFile :: Parser IfoFile
  ifoFile = do
    ifoMagicData    <- magicData
    (_, ifoVersion) <- endOfLine *> pair (Just "version")
    ifoData         <- Map.fromList <$> (endOfLine *> (many (pair Nothing) <* endOfLine))
    return IfoFile {..}

  magicData :: Parser ByteString
  magicData = BS.fromStrict <$> takeWhile (not . isEndOfLine)

  pair :: Maybe ByteString -> Parser (ByteString, ByteString)
  pair = pair' . maybe
    (takeWhile $ inClass "A-Za-z0-9-_")
    (string . BS.toStrict) where

    pair' key = do
      k <- BS.fromStrict <$> (skipSpace *> key)
      _ <- skipSpace *> char '='
      v <- BS.fromStrict <$> (skipSpace *> takeWhile (not . isEndOfLine))
      return (k, v)


type Index = Map ByteString (Integer, Int)
type IndexEntry = (ByteString, (Integer, Int))

data StarDict = StarDict {
    sdIfoFile  :: IfoFile
  , sdIndex    :: Index
  , sdDataPath :: FilePath
  } deriving (Eq, Show)


type IfoFilePath = FilePath

readIndexFile :: (Integral a) => Get a -> IfoFilePath -> IO Index
readIndexFile num fn = getIndexContents >>= return . mkIndex where
  -- TODO check for .gz
  getIndexContents :: IO ByteString
  getIndexContents = BS.readFile (fn -<.> ".idx")

  mkIndex :: ByteString -> Index
  mkIndex = Map.fromList . runGet getIndexEntries

  getIndexEntries :: Get [IndexEntry]
  getIndexEntries = isEmpty >>= \case
    True  -> return []
    False -> liftA2 (:) getIndexEntry getIndexEntries

  getIndexEntry :: Get IndexEntry
  getIndexEntry = (,) <$> getLazyByteStringNul
                      <*> ((,) <$> (fromIntegral <$> num)
                               <*> (fromIntegral <$> num))
-- TODO check for .gz
checkDataFile :: IfoFilePath -> IO FilePath
checkDataFile ifoPath = do
  let dictPath = ifoPath -<.> ".dict"
  b <- doesFileExist dictPath
  when (not b) $ error "TODO"
  return dictPath

mkDictionary :: FilePath -> IO StarDict
mkDictionary ifoPath = do
  sdIfoFile  <- readIfoFile   ifoPath
  sdIndex    <- readIndexFile getWord32be ifoPath
  sdDataPath <- checkDataFile ifoPath
  return StarDict {..}


instance Dictionary StarDict where
  getEntries str (StarDict {..}) = withFile sdDataPath ReadMode extractEntries where

    extractEntries :: Handle -> IO [ByteString]
    extractEntries h = mapM (extractEntry h) . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: Handle -> (Integer, Int) -> IO ByteString
    extractEntry h (offset, size) = hSeek h AbsoluteSeek offset >> BS.hGet h size

