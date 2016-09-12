{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NLP.Dictionary.StarDict (
    StarDict (..)
  , StarDictException (..)
  , mkDictionary
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative (liftA2, many)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, endOfLine, skipSpace, char)
import Data.Binary.Get (Get, runGetOrFail, isEmpty, getLazyByteStringNul, getWord32be)
import Data.Char (chr)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Typeable (Typeable)
import Data.Word
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath.Posix (dropExtension, joinPath, takeBaseName, (-<.>), (<.>))
import System.IO (Handle, IOMode(..), SeekMode(..), withFile, hSeek)
import Data.ByteString.Lazy (ByteString)
import NLP.Dictionary (Dictionary(..))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

data StarDictException
  = WrongIfoFormat FilePath
  | IndexNotFound FilePath
  | WrongIndexFormat FilePath String
  | DictionaryNotFound FilePath
  deriving (Eq, Show, Typeable)

instance Exception StarDictException


data IfoFile = IfoFile {
    ifoMagicData :: ByteString
  , ifoVersion   :: ByteString
  , ifoData      :: Map ByteString ByteString
  } deriving (Eq, Show)

readIfoFile :: (MonadThrow m, MonadIO m) => FilePath -> m IfoFile
readIfoFile ifoPath = (liftIO . BS.readFile $ ifoPath) >>= parseContents where
  parseContents contents = case (parse ifoFile contents) of
    (Fail _ _ _) -> throwM $ WrongIfoFormat ifoPath
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



checkFiles :: IfoFilePath -> [FilePath] -> IO (Maybe FilePath)
checkFiles _ [] = return Nothing
checkFiles ifoPath (ext:exts) = let fn = ifoPath -<.> ext
  in (doesFileExist fn) >>= \case
    True  -> return . Just $ fn
    False -> checkFiles ifoPath exts

checkGZFiles
  :: IfoFilePath
  -> [FilePath]
  -> [FilePath]
  -> IO (Maybe (Either FilePath FilePath))
checkGZFiles ifoPath exts exts' = (checkFiles ifoPath exts) >>= maybe
  (fmap (Right <$>) (checkFiles ifoPath exts'))
  (return . Just . Left)


type IfoFilePath = FilePath

readIndexFile :: (Integral a, MonadThrow m, MonadIO m) => Get a -> IfoFilePath -> m Index
readIndexFile num fn = checkIndexFile fn >>= getIndexContents >>= mkIndex where

  checkIndexFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> m FilePath
  checkIndexFile ifoPath = (liftIO $ checkGZFiles ifoPath ["idx"] ["idx.gz"]) >>= \case
    Nothing         -> throwM $ IndexNotFound ifoPath
    Just (Left fn)  -> return fn
    Just (Right fn) -> error "TODO"

  getIndexContents :: (MonadThrow m, MonadIO m) => FilePath -> m (FilePath, ByteString)
  getIndexContents fn = liftIO . fmap (fn,) . BS.readFile $ fn

  mkIndex :: (MonadThrow m, MonadIO m) => (FilePath, ByteString) -> m Index
  mkIndex (fn, contents) = either
    (\(_, _, err) -> throwM $ WrongIndexFormat fn err)
    (\(_, _, res) -> return . Map.fromList $ res)
    (runGetOrFail getIndexEntries contents)

  getIndexEntries :: Get [IndexEntry]
  getIndexEntries = isEmpty >>= \case
    True  -> return []
    False -> liftA2 (:) getIndexEntry getIndexEntries

  getIndexEntry :: Get IndexEntry
  getIndexEntry = (,) <$> getLazyByteStringNul
                      <*> ((,) <$> (fromIntegral <$> num)
                               <*> (fromIntegral <$> num))

checkDataFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> m FilePath
checkDataFile ifoPath = (liftIO $ checkGZFiles ifoPath ["dict1"] ["dict.dz"]) >>= \case
  Nothing         -> throwM $ DictionaryNotFound ifoPath
  Just (Left fn)  -> return fn
  Just (Right fn) -> liftIO $ do
    fn' <- (joinPath . (:[(takeBaseName ifoPath) <.> "dict"])) <$> getTemporaryDirectory
    GZip.decompress <$> (BS.readFile fn) >>= BS.writeFile fn'
    return fn'

mkDictionary :: (MonadThrow m, MonadIO m) => FilePath -> m StarDict
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

