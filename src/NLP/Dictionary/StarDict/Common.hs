{-|
  Module:      NLP.Dictionary.StarDict.Common
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Common functions and types for StarDict dictionaries.
-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module NLP.Dictionary.StarDict.Common (
    StarDictException (..)

  , checkFiles
  , checkGZFiles

  , IfoFile(..)
  , IfoFilePath
  , ifoDateFormat
  , readIfoFile
  , renderIfoFile

  , IndexEntry
  , readIndexFile
  , renderIndexFile
  , getIndexNumber
  , putIndexNumber

  , checkDataFile

  , DataEntry (..)
  , Renderer
  , mkDataParser

  , StarDict (..)
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative (liftA2, many)
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Attoparsec.ByteString.Lazy (Result(..), Parser, parse, string, takeWhile, inClass)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, endOfLine, skipSpace, char)
import Data.Binary.Get (Get, runGetOrFail, isEmpty)
import Data.Binary.Get (getRemainingLazyByteString, getLazyByteStringNul, getLazyByteString)
import Data.Binary.Get (getWord32be, getWord64be)
import Data.Binary.Builder (Builder, fromLazyByteString, empty, toLazyByteString)
import Data.Binary.Builder (putWord64be, putWord32be, singleton)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Tagged (Tagged)
import Data.Typeable (Typeable)
import Data.Time (parseTimeM, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8, decodeLatin1)
import NLP.Dictionary (Dictionary)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath.Posix (joinPath, takeBaseName, (-<.>), (<.>))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)


-- | Exceptions that are thrown when something with this module went wrong.
data StarDictException
  = WrongIfoFormat FilePath String
  -- ^ Thrown when information file (.ifo) has unsupported format.

  | IndexNotFound FilePath
  -- ^ Thrown when index file (.idx, .idx.gz) is not found.

  | WrongIndexFormat FilePath String
  -- ^ Thrown when index file has unsupported format.

  | DictionaryNotFound FilePath
  -- ^ Thrown when dictionary file (.dict, .dict.dz) has unsupported format.
  deriving (Eq, Show, Typeable)

instance Exception StarDictException


-- | Type synonym to distinguish usage of paths.
type IfoFilePath = FilePath

-- | Representation of .ifo file.
data IfoFile = IfoFile {
    ifoMagicData        :: ByteString     -- ^ Corresponds to the first string in the file.
  , ifoVersion          :: String         -- ^ Corresponds to version field.
  , ifoBookName         :: Text           -- ^ Corresponds to bookname field.
  , ifoWordCount        :: Int            -- ^ Corresponds to wordcount field.
  , ifoIdxFileSize      :: Int            -- ^ Corresponds to idxfilesize field.
  , ifoIdxOffsetBits    :: Maybe Int      -- ^ Corresponds to idxoffsetbits field.
  , ifoSynWordCount     :: Maybe Int      -- ^ Corresponds to synwordcount field.
  , ifoAuthor           :: Maybe Text     -- ^ Corresponds to author field.
  , ifoEmail            :: Maybe Text     -- ^ Corresponds to email field.
  , ifoWebsite          :: Maybe Text     -- ^ Corresponds to website field.
  , ifoDescription      :: Maybe Text     -- ^ Corresponds to description field.
  , ifoDate             :: Maybe UTCTime  -- ^ Corresponds to date field.
  , ifoSameTypeSequence :: Maybe String   -- ^ Corresponds to sametypesequence field.
  , ifoDictType         :: Maybe String
  } deriving (Eq, Show, Generic)

instance NFData IfoFile

-- | Date format of 'ifoDate' in IfoFile.
ifoDateFormat :: String
ifoDateFormat = "%0Y.%m.%d"

-- | Read .ifo file at the given path.
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

    (_, ifoVersion) <- (id *** BSC8.unpack) <$> (endOfLine *> pair (Just "version"))
    expect "version" ifoVersion ["2.4.2", "3.0.0"]

    ifoData <- Map.fromList <$> (endOfLine *> (many (pair Nothing) <* endOfLine))
    let get = flip Map.lookup ifoData
    let require field = ( $ (get field)) $ maybe
          (fail $ "required field " ++ BSC8.unpack field ++ " not found") (return)

    ifoBookName    <- decodeUtf8 <$> require "bookname"
    ifoWordCount   <- read . BSC8.unpack <$> require "wordcount"
    ifoIdxFileSize <- read . BSC8.unpack <$> require "idxfilesize"

    let ifoIdxOffsetBits = read . BSC8.unpack <$> get "idxoffsetbits"
    justExpect "idxoffsetbits" ifoIdxOffsetBits [32, 64]

    let ifoSynWordCount     = read . BSC8.unpack <$> get "synwordcount"
    let ifoAuthor           = decodeUtf8 <$> get "author"
    let ifoEmail            = decodeUtf8 <$> get "email"
    let ifoWebsite          = decodeUtf8 <$> get "website"
    let ifoDescription      = decodeUtf8 <$> get "description"

    let ifoDate = get "date" >>= parseTimeM False defaultTimeLocale ifoDateFormat . BSC8.unpack

    let ifoSameTypeSequence = BSC8.unpack <$> get "sametypesequence"

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

-- | Generates .ifo file contents based on 'IfoFile'
renderIfoFile :: IfoFile -> Text
renderIfoFile IfoFile {..} = T.intercalate "\n" $ [
      decodeUtf8 ifoMagicData
    , "version="     <> (T.pack ifoVersion)
    , "bookname="    <> ifoBookName
    , "wordcount="   <> (T.pack . show $ ifoWordCount)
    , "idxfilesize=" <> (T.pack . show $ ifoIdxFileSize)
  ] ++ catMaybes [
      (("idxoffsetbits="    <>) . T.pack . show)    <$> ifoIdxOffsetBits
    , (("synwordcount="     <>) . T.pack . show)    <$> ifoSynWordCount
    , ("author="            <>)                     <$> ifoAuthor
    , ("email="             <>)                     <$> ifoEmail
    , ("website="           <>)                     <$> ifoWebsite
    , ("description="       <>)                     <$> ifoDescription
    , (("date="             <>) . T.pack
      . formatTime defaultTimeLocale ifoDateFormat) <$> ifoDate
    , (("sametypesequence=" <>) . T.pack)           <$> ifoSameTypeSequence
    , (("dicttype="         <>) . T.pack)           <$> ifoDictType
  ]

-- | Get 32-bit or 64-bit integer depending on description in the .ifo file.
getIndexNumber :: Maybe Int -> Get Int
getIndexNumber = \case
  (Just 64) -> fromIntegral <$> getWord64be
  _         -> fromIntegral <$> getWord32be

-- | Put 32-bit or 64-bit integer depending on description in the .ifo file.
putIndexNumber :: Maybe Int -> (Int -> Builder)
putIndexNumber = \case
  (Just 64) -> putWord64be . fromIntegral
  _         -> putWord32be . fromIntegral

-- | Representation of an .idx file entry.
type IndexEntry = (Text, (Int, Int))

-- | Given .ifo file name and list of extensions, returns first existing file with the same basename.
checkFiles :: IfoFilePath -> [FilePath] -> IO (Maybe FilePath)
checkFiles _ [] = return Nothing
checkFiles ifoPath (ext:exts) = let fn = ifoPath -<.> ext
  in (doesFileExist fn) >>= \case
    True  -> return . Just $ fn
    False -> checkFiles ifoPath exts

-- | Given .ifo file name and two lists of extensions, returns first
-- existing file with with the same basename and extension from the first
-- list or (if such file doesn't exists) from the second list.
checkGZFiles
  :: IfoFilePath
  -> [FilePath]
  -> [FilePath]
  -> IO (Maybe (Either FilePath FilePath))
checkGZFiles ifoPath exts exts' = (checkFiles ifoPath exts) >>= maybe
  (fmap (Right <$>) (checkFiles ifoPath exts'))
  (return . Just . Left)


-- | Read .idx (.idx.gz) file.
readIndexFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> Get Int -> m [IndexEntry]
readIndexFile fn num = checkIndexFile fn >>= getIndexContents >>= mkIndex where

  checkIndexFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> m (Either FilePath FilePath)
  checkIndexFile ifoPath = (liftIO $ checkGZFiles ifoPath ["idx"] ["idx.gz"]) >>= \case
    Nothing   -> throwM $ IndexNotFound ifoPath
    Just path -> return path

  getIndexContents :: (MonadIO m)
    => Either FilePath FilePath -> m (FilePath, ByteString)
  getIndexContents path = liftIO . fmap (fn',) . postprocess . BS.readFile $ fn' where
    postprocess = either (const id) (const $ fmap GZip.decompress) path
    fn' = either id id path

  mkIndex :: (MonadThrow m) => (FilePath, ByteString) -> m [IndexEntry]
  mkIndex (fn', contents) = either
    (\(_, _, err) -> throwM $ WrongIndexFormat fn' err)
    (\(_, _, res) -> return $ res)
    (runGetOrFail getIndexEntries contents)

  getIndexEntries :: Get [IndexEntry]
  getIndexEntries = isEmpty >>= \case
    True  -> return []
    False -> liftA2 (:) getIndexEntry getIndexEntries

  getIndexEntry :: Get IndexEntry
  getIndexEntry = (,) <$> (decodeUtf8 <$> getLazyByteStringNul)
                      <*> ((,) <$> num <*> num)

-- | Generates .idx file contents based on 'Index'.
renderIndexFile :: [IndexEntry] -> (Int -> Builder) -> ByteString
renderIndexFile entries putNum = toLazyByteString $ buildIndex entries where

  buildIndex [] = empty
  buildIndex (e:es) = putEntry e <> buildIndex es

  putEntry (entry, (offset, size)) = foldr1 (<>) [
      (fromLazyByteString . encodeUtf8 $ entry)
    , singleton 0
    , putNum offset
    , putNum size
    ]


-- | Returns path of decompressed dictionary.
checkDataFile :: (MonadThrow m, MonadIO m) => IfoFilePath -> m FilePath
checkDataFile ifoPath = (liftIO $ checkGZFiles ifoPath ["dict"] ["dict.dz"]) >>= \case
  Nothing         -> throwM $ DictionaryNotFound ifoPath
  Just (Left fn)  -> return fn
  Just (Right fn) -> liftIO $ do
    fn' <- (joinPath . (:[(takeBaseName ifoPath) <.> "dict"])) <$> getTemporaryDirectory
    GZip.decompress <$> (BS.readFile fn) >>= BS.writeFile fn'
    return fn'


-- | Possible dictionary entry formats.
data DataEntry
  = UTF8Text Text
  | LocaleText Text
  | Pango Text
  | Phonetics Text
  | XDXF Text
  | CJK Text
  | PowerWord Text
  | MediaWiki Text
  | HTML Text
  | Resource [FilePath]
  | WAVEAudio ByteString
  | Picture ByteString
  | Reserved ByteString
  deriving (Eq, Show)

-- | Parser for a list of elements.
getMany :: Get a -> Get [a]
getMany p = isEmpty >>= \case
  True  -> return []
  False -> liftA2 (:) p (getMany p)

-- | Returns parser based on description in .ifo file.
mkDataParser :: Maybe String -> Get [DataEntry]
mkDataParser = maybe (getMany getGenericEntry) getSpecificEntries where

  getGenericEntry :: Get DataEntry
  getGenericEntry = BSC8.head <$> getLazyByteString 1
                >>= getSpecificEntry getLazyByteStringNul

  getSpecificEntries :: [Char] -> Get [DataEntry]
  getSpecificEntries cs = sequence $ zipWith getSpecificEntry ps cs where
    ps :: [Get ByteString]
    ps = reverse . take (length cs) $ getRemainingLazyByteString:(repeat getLazyByteStringNul)

  getSpecificEntry :: Get ByteString -> Char -> Get DataEntry
  getSpecificEntry getData = \case
    'm' -> UTF8Text   . decodeUtf8 <$> getData
    'l' -> LocaleText . decodeLatin1 <$> getData
    'g' -> Pango      . decodeUtf8 <$> getData
    't' -> Phonetics  . decodeUtf8 <$> getData
    'x' -> XDXF       . decodeUtf8 <$> getData
    'y' -> CJK        . decodeUtf8 <$> getData
    'k' -> PowerWord  . decodeUtf8 <$> getData
    'w' -> MediaWiki  . decodeUtf8 <$> getData
    'h' -> HTML       . decodeUtf8 <$> getData
    'n' -> Resource   . lines . T.unpack . decodeUtf8 <$> getData
    'r' -> Resource   . lines . T.unpack . decodeUtf8 <$> getData
    'W' -> WAVEAudio <$> getData
    'P' -> Picture   <$> getData
    'X' -> Reserved  <$> getData
    _   -> error "type not supported"

-- | Type of function to transform dictionary entries to a text.
type Renderer = DataEntry -> Text


-- | Classtype for stardict dictionaries.
class (Dictionary d) => StarDict d where
  getIfoFile   :: d -> IfoFile
  mkDictionary :: (MonadThrow m, MonadIO m) => Tagged d IfoFilePath -> Renderer -> m d
