{-# LANGUAGE OverloadedStrings #-}

import NLP.Dictionary (Dictionary(..))
import qualified NLP.Dictionary.StarDict as StarDict

main :: IO ()
main = do
  dict <- StarDict.mkDictionary "../../references/wordnet-stardict/dictd_www.dict.org_wn.ifo"
  putStrLn . show  =<< (getEntries "monad" dict)
