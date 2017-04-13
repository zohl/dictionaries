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


mkName :: Bool -> Int -> Int -> String
mkName inMemory wordsNum wordsSize = intercalate "_" $ [
    if inMemory then "InMemory" else "Regular"
  , show wordsNum
  , show wordsSize
  ]

mkDictionary :: (MonadIO m, MonadThrow m) => Bool -> SD.IfoFilePath -> m DictionaryWrapper
mkDictionary inMemory = if inMemory
  then \path -> wrapDictionary <$> SDIM.mkDictionary path renderId
  else \path -> wrapDictionary <$> SD.mkDictionary path renderId

  
benchLoading :: Bool -> Int -> Int -> Benchmark
benchLoading inMemory wordsNum wordsSize = env
  (generateDictionary >>= generateStarDict) -- TODO add parameters
  $ \starDictPath -> bench (mkName inMemory wordsNum wordsSize) $ do
    nfIO $ (mkDictionary inMemory starDictPath)


main :: IO ()
main = defaultMain [
    bgroup "Loading" [
      benchLoading False 0 0 
    , benchLoading True  0 0 
    ]
  , bgroup "Accessing" [] -- TODO add benchmarks
  ]
