{-# LANGUAGE TypeSynonymInstances #-}

module Tests.Vision.Image (
      tests
    ) where

import Control.Applicative
import Data.Int
import Test.QuickCheck

import Data.Array.Repa (toList)

import Vision.Primitive (Point (..), Size (..), sizeRange)
import qualified Vision.Image.RGBAImage as R
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II

import Tests.Config (maxImageSize)
import qualified Tests.Vision.Primitive

instance Arbitrary R.RGBAImage where
    arbitrary = do
        size@(Size w h) <- arbitrary
        R.create size <$> vector (w * h * 4)
        
instance Arbitrary G.GreyImage where
    arbitrary = do
        size@(Size w h) <- arbitrary
        G.create size <$> vector (w * h)
        
instance Arbitrary II.IntegralImage where
    arbitrary =
        II.integralImage <$> arbitrary <*> return id

tests = conjoin [
              label "Grey to/from RGBA" $ propGreyRGBA
            , label "RGBA resize" $ propRGBAResize
            , label "Grey resize" $ propGreyResize
            , label "Integral last pixel" $ propIntegralLastPixel
            , label "Integral values" $ propIntegralPixels
        ]

-- | Tests if the converts greyscale to and from RGBA gives the same image.
propGreyRGBA :: G.GreyImage -> Bool
propGreyRGBA img = img == G.fromRGBA (G.toRGBA img)

-- | Tests if by increasing the size of the image by a factor of two and
-- reduce by two gives the original image.
propRGBAResize :: R.RGBAImage -> Bool
propRGBAResize img =
    let size@(Size w h) = R.getSize img
    in img == R.resize (R.resize img (Size (w * 2) (h * 2))) size

-- | Tests if the last pixel of the 'IntegralImage' is the same as the sum of
-- the image\'s pixels.
propGreyResize :: G.GreyImage -> Bool
propGreyResize img =
    let size@(Size w h) = G.getSize img
    in img == G.resize (G.resize img (Size (w * 2) (h * 2))) size

-- | Tests if the last pixel of the 'IntegralImage' is the same as the sum of
-- the image\'s pixels.
propIntegralLastPixel :: G.GreyImage -> Bool
propIntegralLastPixel img =
    lastIntegral == sumImage
  where
    integral = II.integralImage img id
    size@(Size w h) = G.getSize img
    lastIntegral = integral `II.getValue` Point w h
    sumImage = sum $ map int64 $ toList img

-- | Tests if all pixels from the 'IntegralImage' gives the correct sum.
propIntegralPixels :: G.GreyImage -> Bool
propIntegralPixels img =
    and [ currectSum point | point <- sizeRange (Size (w+1) (h+1)) ]
  where
    size@(Size w h) = G.getSize img
    integral = II.integralImage img id
    currectSum point@(Point x y) =
        let imageSum = sum $ map getPixel (sizeRange (Size x y))
            integralSum = integral `II.getValue` point
        in imageSum == integralSum
    getPixel = int64 . (img `G.getPixel`)

int64 :: Integral a => a -> Int64
int64 = fromIntegral    