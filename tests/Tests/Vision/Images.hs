module Tests.Vision.Images (
      tests
    ) where

import Control.Applicative
import Test.QuickCheck

import Tests.Config (maxImageSize)

import Vision.Images.Image (Image)

instance Arbitrary Image where
    arbitrary =
        Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)
        
instance Arbitrary GreyImage where
    arbitrary =
        Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)
        
instance Arbitrary IntegralImage where
    arbitrary =
        Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)

tests = label "sizeRange length" propSizeRangeLength

propSizeRangeLength :: Size -> Bool
propSizeRangeLength size@(Size w h) =
    length (sizeRange size) == w * h 
