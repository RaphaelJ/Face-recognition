module Vision.Image.IntegralImage (
    -- * Type
      IntegralImage
    -- * Functions
    , integralImage, getValue, getSize
    ) where

import Debug.Trace

import Data.Array (Array, (!), array, bounds, elems)
import Data.Int

import qualified Vision.Image.GreyImage as G
import Vision.Primitive (Point (..), Size (..))

type IntegralImage = Array (Int, Int) Int64

-- | Computes an 'IntegralImage' using a transformation function on each pixel.
integralImage :: Integral a => G.GreyImage -> (G.Pixel -> a) -> IntegralImage
integralImage image f =
    integral
  where
    integral = array ((0, 0), (h, w)) (topValues ++ leftValues ++ values)

    -- Initializes the first row and the first column to zero
    topValues = scanl (\acc x -> ((0, x), 0)) ((0, 0), 0) [1..w]
    leftValues = scanl (\acc y -> ((y, 0), 0)) ((0, 0), 0) [1..h]

    values = [ ((y, x), pix + left + top - topLeft) |
          y <- [1..h], x <- [1..w]
        , let pix = value (x-1) (y-1)
        , let topLeft = integral ! (y-1, x-1)
        , let top = integral ! (y-1, x)
        , let left = integral ! (y, x-1)
        ]
    
    Size w h = G.getSize image
    value x y = int64 $! f $! image `G.getPixel` Point x y
{-# INLINABLE integralImage #-}

-- | Gets the value of a point inside an 'IntegralImage'. A value with x or y
-- equals to 0 will ever be 0.
getValue :: IntegralImage -> Point -> Int64
getValue image (Point x y) = image ! (y, x)
{-# INLINE getValue #-}

-- | Gives the original image\'s size.
getSize :: IntegralImage -> Size
getSize image =
    let (h, w) = snd $ bounds $ image
    in Size w h
{-# INLINE getSize #-}

int64 :: Integral a => a -> Int64
int64 = fromIntegral