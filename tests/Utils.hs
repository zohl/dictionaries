{-# LANGUAGE OverloadedStrings #-}

module Utils (
    generateDictionary
  , generateStarDict
  , renderId
  ) where

import Control.Arrow (second)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder as BT
import Data.Traversable (mapAccumL)
import NLP.Dictionary.StarDict.Common (IfoFile(..), IndexEntry, Renderer, DataEntry(..))
import NLP.Dictionary.StarDict.Common (renderIfoFile, renderIndexFile, putIndexNumber)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.Random (getStdRandom, Random(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T


randomString :: Int -> IO String
randomString n = mapM
  (const $ getStdRandom (randomR ('a', 'z')))
  (replicate n ())

randomWord :: (Int, Int) -> IO T.Text
randomWord wordSize = (getStdRandom $ randomR wordSize) >>= (fmap T.pack . randomString)

randomText :: (Int, Int) -> (Int, Int) -> IO T.Text
randomText textSize wordSize =
  fmap (("<<" <>) . (<> ">>"))
  $ (getStdRandom $ randomR textSize)
  >>= fmap (T.intercalate " ") . sequence . flip replicate (randomWord wordSize)


generateDictionary :: Int -> (Int, Int) -> (Int, Int) -> IO [(T.Text, T.Text)]
generateDictionary dictionarySize textSize wordSize = do

  ws <- fmap (Set.toList . Set.fromList) $
          sequence $ replicate dictionarySize (randomWord wordSize)

  zip ws <$> (sequence $ replicate (length ws) (randomText textSize wordSize))


generateIndex :: [(T.Text, T.Text)] -> [IndexEntry]
generateIndex = snd
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
        . foldr1 (<>)
        . ((BT.fromLazyText ""):)
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


renderId :: Renderer
renderId (UTF8Text s) = s
renderId _ = error "not implemented"
