{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NLP.Dictionary.StarDict (
    StarDict (..)
  , StarDictException (..)
  , mkDictionary

  , checkFiles
  , checkGZFiles

  , IfoFile(..)
  , IfoFilePath
  , readIfoFile
  , indexNumberParser

  , Index
  , IndexEntry
  , readIndexFile

  , checkDataFile
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative (liftA2, many)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, endOfLine, skipSpace, char)
import Data.Binary.Get (Get, runGetOrFail, isEmpty)
import Data.Binary.Get (getLazyByteStringNul, getWord32be, getWord64be)
import Data.Char (chr)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Typeable (Typeable)
import Data.Time (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath.Posix (dropExtension, joinPath, takeBaseName, (-<.>), (<.>))
import System.IO (Handle, IOMode(..), SeekMode(..), withFile, hSeek)
import Data.ByteString.Lazy (ByteString)
import NLP.Dictionary (Dictionary(..))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.Map.Strict as Map

data StarDictException
  = WrongIfoFormat FilePath String
  | IndexNotFound FilePath
  | WrongIndexFormat FilePath String
  | DictionaryNotFound FilePath
  deriving (Eq, Show, Typeable)

instance Exception StarDictException


data IfoFile = IfoFile {
    ifoMagicData        :: ByteString
  , ifoVersion          :: ByteString
  , ifoBookName         :: ByteString
  , ifoWordCount        :: Int
  , ifoIdxFileSize      :: Int
  , ifoIdxOffsetBits    :: Maybe Int
  , ifoSynWordCount     :: Maybe Int
  , ifoAuthor           :: Maybe ByteString
  , ifoEmail            :: Maybe ByteString
  , ifoWebsite          :: Maybe ByteString
  , ifoDescription      :: Maybe ByteString
  , ifoDate             :: Maybe UTCTime
  , ifoSametypeSequence :: Maybe String
  , ifoDictType         :: Maybe String
  } deriving (Eq, Show)

readIfoFile :: (MonadThrow m, MonadIO m) => FilePath -> m IfoFile
readIfoFile ifoPath = (liftIO . BS.readFile $ ifoPath) >>= parseContents where
  parseContents contents = case (parse ifoFile contents) of
    (Fail _ _ msg) -> throwM $ WrongIfoFormat ifoPath msg
    (Done _ r)     -> return r

  expect :: (Eq a, Show a) => String -> a -> [a] -> Parser ()
  expect name x xs = unless (x `elem` xs) . fail . concat $ [
      name, " must be ", fmts xs, " (", show x, " provided)"
    ] where
      fmt y = '\'':(show y) ++ "'"

      fmts = \case
        []     -> ""
        (y:[]) -> fmt y
        ys     -> (intercalate ", " . map fmt . init $ ys) ++ " or " ++ (fmt . last $ ys)

  justExpect :: (Eq a, Show a) => String -> Maybe a -> [a] -> Parser ()
  justExpect name mx xs = maybe (return ()) (\x -> expect name x xs) mx

  ifoFile :: Parser IfoFile
  ifoFile = do
    ifoMagicData <- magicData
    expect "magic data" ifoMagicData ["StarDict's dict ifo file"]

    (_, ifoVersion) <- endOfLine *> pair (Just "version")
    expect "version" ifoVersion ["2.4.2", "3.0.0"]

    ifoData <- Map.fromList <$> (endOfLine *> (many (pair Nothing) <* endOfLine))
    let get = flip Map.lookup ifoData
    let require field = ( $ (get field)) $ maybe
          (fail $ "required field " ++ BSC8.unpack field ++ " not found") (return)

    ifoBookName    <- require "bookname"
    ifoWordCount   <- read . BSC8.unpack <$> require "wordcount"
    ifoIdxFileSize <- read . BSC8.unpack <$> require "idxfilesize"

    let ifoIdxOffsetBits = read . BSC8.unpack <$> get "idxoffsetbits"
    justExpect "idxoffsetbits" ifoIdxOffsetBits [32, 64]

    let ifoSynWordCount     = read . BSC8.unpack <$> get "synwordcount"
    let ifoAuthor           = get "author"
    let ifoEmail            = get "email"
    let ifoWebsite          = get "website"
    let ifoDescription      = get "description"

    let ifoDate = get "date" >>= parseTimeM False defaultTimeLocale "%0Y.%m.%d" . BSC8.unpack

    let ifoSametypeSequence = BSC8.unpack <$> get "sametypesequence"

    let ifoDictType = BSC8.unpack <$> get "dicttype"
    justExpect "dicttype" ifoDictType ["wordnet"]

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

indexNumberParser :: IfoFile -> Get Int
indexNumberParser IfoFile {..} = case ifoIdxOffsetBits of
  (Just 64) -> fromIntegral <$> getWord64be
  _         -> fromIntegral <$> getWord32be

type Index = Map ByteString (Int, Int)
type IndexEntry = (ByteString, (Int, Int))

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

readIndexFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> Get Int -> m Index
readIndexFile fn num = checkIndexFile fn >>= getIndexContents >>= mkIndex where

  checkIndexFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> m (Either FilePath FilePath)
  checkIndexFile ifoPath = (liftIO $ checkGZFiles ifoPath ["idx"] ["idx.gz"]) >>= \case
    Nothing   -> throwM $ IndexNotFound ifoPath
    Just path -> return path

  getIndexContents :: (MonadThrow m, MonadIO m)
    => Either FilePath FilePath -> m (FilePath, ByteString)
  getIndexContents path = liftIO . fmap (fn,) . postprocess . BS.readFile $ fn where
    postprocess = either (const id) (const $ fmap GZip.decompress) path
    fn = either id id path

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
                      <*> ((,) <$> num <*> num)

checkDataFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> m FilePath
checkDataFile ifoPath = (liftIO $ checkGZFiles ifoPath ["dict1"] ["dict.dz"]) >>= \case
  Nothing         -> throwM $ DictionaryNotFound ifoPath
  Just (Left fn)  -> return fn
  Just (Right fn) -> liftIO $ do
    fn' <- (joinPath . (:[(takeBaseName ifoPath) <.> "dict"])) <$> getTemporaryDirectory
    GZip.decompress <$> (BS.readFile fn) >>= BS.writeFile fn'
    return fn'


data StarDict = StarDict {
    sdIfoFile  :: IfoFile
  , sdIndex    :: Index
  , sdDataPath :: FilePath
  } deriving (Eq, Show)

mkDictionary :: (MonadThrow m, MonadIO m) => IfoFilePath -> m StarDict
mkDictionary ifoPath = do
  sdIfoFile  <- readIfoFile   ifoPath
  sdIndex    <- readIndexFile ifoPath (indexNumberParser sdIfoFile)
  sdDataPath <- checkDataFile ifoPath
  return StarDict {..}

instance Dictionary StarDict where
  getEntries str (StarDict {..}) = withFile sdDataPath ReadMode extractEntries where

    extractEntries :: Handle -> IO [ByteString]
    extractEntries h = mapM (extractEntry h) . maybeToList . Map.lookup str $ sdIndex

    extractEntry :: Handle -> (Int, Int) -> IO ByteString
    extractEntry h (offset, size) = hSeek h AbsoluteSeek offset' >> BS.hGet h size where
      offset' = fromIntegral offset

