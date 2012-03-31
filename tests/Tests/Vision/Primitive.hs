module Tests.Vision.Primitive (
      tests
    ) where

import Control.Applicative

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Vision.Primitive (Size (..), sizeRange)

import Tests.Config (maxImageSize)

instance Arbitrary Size where
    arbitrary =
        Size <$> choose (1, maxImageSize) <*> choose (1, maxImageSize)

tests = [
      testProperty "sizeRange length" propSizeRangeLength
    ]

propSizeRangeLength :: Size -> Bool
propSizeRangeLength size@(Size w h) =
    length (sizeRange size) == w * h