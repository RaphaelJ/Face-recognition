module Tests.Vision.Primitives (
      primitivesTests
    ) where

import Control.Applicative
import Test.QuickCheck

import Tests.Config (maxImageSize)

import Vision.Primitives (Size (..), sizeRange)

instance Arbitrary Size where
    arbitrary =
        Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)

primitivesTests = label "sizeRange length" propSizeRangeLength

propSizeRangeLength :: Size -> Bool
propSizeRangeLength size@(Size w h) =
    length (sizeRange size) == w * h