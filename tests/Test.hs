{-# LANGUAGE OverloadedStrings #-}

import NLP.Dictionary (getEntries)
import NLP.Dictionary.StarDict (Renderer, DataEntry(..), mkDictionary)
import Utils (generateDictionary, generateStarDict)
import Test.Hspec (Spec, hspec, describe, it, context, shouldBe)


renderId :: Renderer
renderId (UTF8Text s) = s
renderId _ = error "not implemented"


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RandomDictionary" randomDictionary


randomDictionary :: Spec
randomDictionary = do

  let generateDictionaries = do
        dictionary <- generateDictionary
        starDictPath <- generateStarDict dictionary
        starDict <- mkDictionary starDictPath renderId
        return (dictionary, starDict)

  context "when getting entries" $ do
    it "matches every entry in the dictionary" $ do
      (dictionary, starDict) <- generateDictionaries
      mapM_ (\(name, entry) -> shouldBe [entry] =<< getEntries name starDict) dictionary

    it "doesn't match missing entry" $ do
      (_, starDict) <- generateDictionaries
      shouldBe [] =<< getEntries "# not found" starDict

