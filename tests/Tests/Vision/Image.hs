{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.Vision.Image (
      tests
    ) where

import Debug.Trace
    
import Data.Int

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Vision.Primitive (Point (..), Size (..), sizeRange)
import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II

import Tests.Config (maxImageSize)
import Tests.Vision.Primitive ()

instance (I.Image i p a, Arbitrary a) => Arbitrary i where
    arbitrary = do
        size@(Size w h) <- arbitrary
        xs <- sequence $ take (w*h) $ repeat arbitraryPix
        return $ I.fromList size xs 
      where
        arbitraryPix = do
            vals <- sequence $ repeat arbitrary
            return $ I.valuesToPix vals

tests = [
--       testProperty "Grey to/from RGBA" $ propGreyRGBA
--     , testProperty "Grey to/from RGB" $ propGreyRGB
    {-, -}testProperty "RGB to/from RGBA" $ propRGBRGBA
--     , testProperty "Image resize Grey" $
--         (propImageResize :: I.GreyImage -> Bool)
--     , testProperty "Image resize RGBA" $
--         (propImageResize :: I.RGBAImage -> Bool)
--     , testProperty "Image resize RGB" $
--         (propImageResize :: I.RGBImage -> Bool)
--     , testProperty "Integral values" $ propIntegralPixels
    ]

-- | Tests if the converts a greyscale to and from RGBA gives the same image.
propGreyRGBA :: I.GreyImage -> Bool
propGreyRGBA image =
    image == I.convert (I.convert image :: I.RGBAImage)
    
-- | Tests if the converts a greyscale to and from RGB gives the same image.
propGreyRGB :: I.GreyImage -> Bool
propGreyRGB image =
    image == I.convert (I.convert image :: I.RGBImage)

-- | Tests if the converts a RGB to and from RGBA gives the same image.
propRGBRGBA :: I.RGBImage -> Bool
propRGBRGBA image =
    image == I.convert (I.convert image :: I.RGBAImage)

-- | Tests if by increasing the size of the image by a factor of two and
-- reduce by two gives the original image.
propImageResize :: (Eq i, I.Image i p a) => i -> Bool
propImageResize image =
    let size@(Size w h) = I.getSize image
    in image == I.resize (I.resize image (Size (w * 2) (h * 2))) size

-- | Tests if all pixels from the 'IntegralImage' give the correct sum.
propIntegralPixels :: I.GreyImage -> Bool
propIntegralPixels image =
    and [ currectSum point | point <- sizeRange $ I.getSize integral ]
  where
    integral = II.integralImage image (^2)
    currectSum point@(Point x y) =
        let imageSum = sum $ [ (int64 $ image `I.getPixel` p)^2 |
                  p <- sizeRange $ Size x y
                ]
            integralSum = integral `I.getPixel` point
        in imageSum == integralSum

int64 :: Integral a => a -> Int64
int64 = fromIntegral