module Tests.Vision.Image (
      tests
    ) where

import Debug.Trace
    
import Data.Int

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Vision.Primitive (Point (..), Size (..), sizeRange)
import qualified Vision.Image as I
import qualified Vision.Image.RGBAImage as R
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II

import Tests.Config (maxImageSize)
import Tests.Vision.Primitive ()

instance Arbitrary G.GreyImage where
    arbitrary = do
        size@(Size w h) <- arbitrary
        xs <- vector (w * h)
        return $ I.fromList size xs

instance Arbitrary R.RGBAImage where
    arbitrary = do
        size@(Size w h) <- arbitrary
        xs <- vector (w * h)
        return $ I.fromList size xs

instance Arbitrary R.Pixel where
    arbitrary = do
        [r, g, b] <- vector 3
        return $ R.Pixel r g b 255

-- instance Arbitrary II.IntegralImage where
--     arbitrary =
--         II.integralImage <$> arbitrary <*> return id

tests = [
      testProperty "Grey to/from RGBA" $ propGreyRGBA
    , testProperty "Image resize RGBA" $
        (propImageResize :: R.RGBAImage -> Bool)
    , testProperty "Image resize Grey" $
        (propImageResize :: G.GreyImage -> Bool)
    , testProperty "Integral values" $ propIntegralPixels
    ]

-- | Tests if the converts greyscale to and from RGBA gives the same image.
propGreyRGBA :: G.GreyImage -> Bool
propGreyRGBA image =
    image == G.fromRGBA (G.toRGBA image)

-- | Tests if by increasing the size of the image by a factor of two and
-- reduce by two gives the original image.
propImageResize :: (Eq i, I.Image i p) => i -> Bool
propImageResize image =
    let size@(Size w h) = I.getSize image
    in image == I.resize (I.resize image (Size (w * 2) (h * 2))) size

-- | Tests if all pixels from the 'IntegralImage' give the correct sum.
propIntegralPixels :: G.GreyImage -> Bool
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