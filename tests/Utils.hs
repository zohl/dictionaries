{-# LANGUAGE OverloadedStrings #-}

module Utils (
    generateDictionary
  , generateStarDict
  ) where

import Control.Arrow (second)
import NLP.Dictionary.StarDict
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.Random (getStdRandom, Random(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString.Lazy as BS
import Data.Traversable (mapAccumL)
import Data.Text.Lazy.Builder as BT
import Data.Monoid ((<>))


randomString :: Int -> IO String
randomString n = mapM
  (const $ getStdRandom (randomR ('a', 'z')))
  (replicate n ())

randomWord :: (Int, Int) -> IO T.Text
randomWord wordSize = (getStdRandom $ randomR wordSize) >>= (fmap T.pack . randomString)

randomText :: (Int, Int) -> (Int, Int) -> IO T.Text
randomText wordSize textSize =
  fmap (("<<" <>) . (<> ">>"))
  $ (getStdRandom $ randomR textSize)
  >>= fmap (T.intercalate " ") . sequence . flip replicate (randomWord wordSize)


generateDictionary :: IO [(T.Text, T.Text)]
generateDictionary = do
  let wordSize = (4, 8) :: (Int, Int)
  let textSize = (4, 8) :: (Int, Int)

  numWords <- getStdRandom $ randomR (3, 40) :: IO Int

  ws <- fmap (Set.toList . Set.fromList) $
          sequence $ replicate numWords (randomWord wordSize)

  zip ws <$> (sequence $ replicate (length ws) (randomText wordSize textSize))


generateIndex :: [(T.Text, T.Text)] -> Index
generateIndex = Map.fromList . snd
  . mapAccumL generateIndexEntry  0
  . map (second (fromIntegral . T.length)) where

    generateIndexEntry :: Int -> (T.Text, Int) -> (Int, IndexEntry)
    generateIndexEntry offset (name, size) = let offset' = offset + (fromIntegral . T.length $ name) in
      (offset' + size, (name, (offset', size)))


generateStarDict :: [(T.Text, T.Text)] -> IO FilePath
generateStarDict dictionary = do
  dictionaryName <- randomString 16
  basePath <- (</>) <$> getTemporaryDirectory <*> (pure dictionaryName)
  createDirectoryIfMissing True basePath

  let dictionaryPath = basePath </> dictionaryName <.> "dict"
  let dictionaryContents = BT.toLazyText
        . foldl1 (<>)
        . map (\(name, entry) -> BT.fromLazyText name <> BT.fromLazyText entry)
        $ dictionary
  T.writeFile dictionaryPath dictionaryContents

  let indexPath = basePath </> dictionaryName <.> "idx"
  let indexContents = renderIndexFile (generateIndex dictionary) (putIndexNumber Nothing)
  BS.writeFile indexPath indexContents

  let ifoPath = basePath </> dictionaryName <.> "ifo"
  let ifoFile = IfoFile {
        ifoMagicData        = "StarDict's dict ifo file"
      , ifoVersion          = "3.0.0"
      , ifoBookName         = "Random Dictionary"
      , ifoWordCount        = length dictionary
      , ifoIdxFileSize      = fromIntegral . BS.length $ indexContents
      , ifoIdxOffsetBits    = Nothing
      , ifoSynWordCount     = Nothing
      , ifoAuthor           = Nothing
      , ifoEmail            = Nothing
      , ifoWebsite          = Nothing
      , ifoDescription      = Nothing
      , ifoDate             = Nothing
      , ifoSameTypeSequence = Just "m"
      , ifoDictType         = Nothing
      }
  T.writeFile ifoPath ((renderIfoFile ifoFile) <> "\n")

  return ifoPath
