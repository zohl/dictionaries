{-|
  Module:      NLP.Dictionary.StarDict
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description
  Tools for StarDict dictionaries.
  This module exports 'NLP.Dictionary.StarDict.Regular' as default
  implementation of a dictionary. It's recommended to user qualified
  imports for dictionary implementations.
-}


module NLP.Dictionary.StarDict (
    StarDict(..)
  , mkDictionary

  , DataEntry(..)
  , Renderer
  ) where

import NLP.Dictionary.StarDict.Regular (StarDict(..), mkDictionary)
import NLP.Dictionary.StarDict.Common (DataEntry(..), Renderer)
