{-# LANGUAGE OverloadedStrings #-}

import NLP.Dictionary (getEntries)
import NLP.Dictionary.StarDict (mkDictionary)
import Test.Hspec (Spec, hspec, describe, it, context, shouldBe)
import Utils (generateDictionary, generateStarDict, renderId)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RandomDictionary" randomDictionary


randomDictionary :: Spec
randomDictionary = do

  let generateDictionaries = do
        dictionary <- generateDictionary 10 (5, 15) (1, 10)
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

