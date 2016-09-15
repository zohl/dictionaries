{-# LANGUAGE OverloadedStrings #-}

import NLP.Dictionary (Dictionary(..))
import qualified NLP.Dictionary.StarDict as StarDict
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)

main :: IO ()
main = do
  -- dict <- StarDict.mkDictionary "../../references/wordnet-stardict/dictd_www.dict.org_wn.ifo"
  dict <- StarDict.mkDictionary
    "../../references/lingvo-universal-stardict/LingvoUniversal.ifo"

  putStrLn . unlines . map (T.unpack . decodeUtf8) =<< (getEntries "monad" dict)
