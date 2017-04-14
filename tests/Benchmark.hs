{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Criterion (Benchmark, bench, nfIO, env)
import Criterion.Main (bgroup, defaultMain)
import Data.List (intercalate)
import NLP.Dictionary
import NLP.Dictionary.StarDict ()
import System.Random (getStdGen)
import System.Random.Shuffle (shuffle')
import Utils (generateDictionary, generateStarDict, renderId)
import qualified NLP.Dictionary.StarDict as SD
import qualified NLP.Dictionary.StarDict.InMemory as SDIM


data DictionaryWrapper = forall d. (NFData d, Dictionary d) => WrapDictionary d

wrapDictionary :: (NFData d, Dictionary d) => d -> DictionaryWrapper
wrapDictionary = WrapDictionary

instance Dictionary DictionaryWrapper where
  getEntries name (WrapDictionary d) = getEntries name d

instance NFData DictionaryWrapper where
  rnf (WrapDictionary !_) = ()


data DictionaryType
  = Regular
  | InMemory
  deriving (Eq, Show, Enum, Bounded)

mkDictionary :: (MonadIO m, MonadThrow m)
  => DictionaryType
  -> SD.IfoFilePath
  -> m DictionaryWrapper
mkDictionary Regular = \p -> wrapDictionary <$> SD.mkDictionary p renderId
mkDictionary InMemory = \p -> wrapDictionary <$> SDIM.mkDictionary p renderId


around :: Int -> (Int, Int)
around x = let dx = min (x `div` 10) 1 in (x - dx, x + dx)

mkName :: Int -> Int -> Int -> String
mkName dictionarySize textSize wordSize = intercalate "_" $ [
    show dictionarySize
  , show textSize
  , show wordSize
  ]


benchLoading :: Int -> Int -> Int -> DictionaryType -> Benchmark
benchLoading dictionarySize textSize wordSize dictionaryType = env
  (generateDictionary
    dictionarySize
    (around textSize)
    (around wordSize)
    >>= generateStarDict)
  $ \starDictPath -> bench (mkName dictionarySize textSize wordSize) $ do
    nfIO $ (mkDictionary dictionaryType starDictPath)


benchAccessing :: Int -> Int -> Int -> DictionaryType -> Benchmark
benchAccessing dictionarySize textSize wordSize dictionaryType = env
  (do
    dict <- generateDictionary
              dictionarySize
              (around textSize)
              (around wordSize)

    sampleWords <- fmap (take 10)
                 . fmap (shuffle' (map fst dict) (length dict))
                 $ getStdGen

    starDict <- generateStarDict dict >>= mkDictionary dictionaryType

    return (starDict, sampleWords))

  $ \ ~(starDict, sampleWords) -> bench (mkName dictionarySize textSize wordSize) $ do
    nfIO $ mapM (flip getEntries starDict) sampleWords


benchDictionaries :: ([DictionaryType -> Benchmark]) -> [Benchmark]
benchDictionaries bs = map
  (\dt -> bgroup (show dt) (map ($ dt) bs))
  [minBound..maxBound]


main :: IO ()
main = defaultMain [
    bgroup "Loading" . benchDictionaries $ [
        benchLoading 100  100  100
      , benchLoading 500  100  100
      , benchLoading 100  500  100
      , benchLoading 100  100  500
      ]
  , bgroup "Accessing" . benchDictionaries $ [
        benchAccessing 100  100  100
      , benchAccessing 500  100  100
      , benchAccessing 100  500  100
      , benchAccessing 100  100  500
      ]
  ]

