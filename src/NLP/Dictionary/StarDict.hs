{-|
  Module:      NLP.Dictionary.StarDict
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Tools for StarDict dictionaries.
  This module (re)exports core classes that are needed to work with
  dictionaries. To create dictionary, use `mkDictionary` method
  together with `tag` from one of implementations:

  @
    import NLP.Dictionary.StarDict (StarDict(..))
    import qualified NLP.Dictionery.StarDict.Regular as SDR

    ...

      dictionary <- mkDictionary (SDR.tag "/path/to/a/dictionary.ifo") renderer
  @
-}


module NLP.Dictionary.StarDict (
    DataEntry(..)
  , Renderer
  , StarDict(..)
  ) where

import NLP.Dictionary.StarDict.Common (StarDict(..), DataEntry(..), Renderer)
