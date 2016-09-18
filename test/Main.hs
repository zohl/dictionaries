{-# LANGUAGE OverloadedStrings #-}

import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (DataEntry(..), Renderer)
import qualified Data.Text.Lazy as T
import qualified NLP.Dictionary.StarDict as StarDict


render :: Renderer
render (UTF8Text s) = s
render (XDXF s) = s
render _ = error "not supported"

main :: IO ()
main = do
  let word = "monad"

  dict1 <- StarDict.mkDictionary
    "../../references/wordnet-stardict/dictd_www.dict.org_wn.ifo"
    render
  putStrLn . unlines . map T.unpack =<< (getEntries word dict1)

  dict2 <- StarDict.mkDictionary
    "../../references/lingvo-universal-stardict/LingvoUniversal.ifo"
    render
  putStrLn . unlines . map T.unpack =<< (getEntries word dict2)
