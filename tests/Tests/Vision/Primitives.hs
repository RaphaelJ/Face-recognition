module Tests.Vision.Primitives (
      tests
    ) where

import Control.Applicative
import Test.QuickCheck

import Vision.Primitive (Size (..), sizeRange)

import Tests.Config (maxImageSize)

instance Arbitrary Size where
    arbitrary =
        Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)

tests = label "sizeRange length" propSizeRangeLength

propSizeRangeLength :: Size -> Bool
propSizeRangeLength size@(Size w h) =
    length (sizeRange size) == w * h