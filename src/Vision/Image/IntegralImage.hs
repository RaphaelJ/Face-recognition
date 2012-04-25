{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.IntegralImage (
    -- * Type
      IntegralImage (..), Pixel
    -- * Functions 
    , integralImage, sumRectangle, imageShape
    ) where

import Debug.Trace

import Data.Array (Array, (!), array, listArray, bounds, elems)
import qualified Data.Array.Repa as R
import Data.Int

import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G
import Vision.Primitive (Point (..), Size (..), Rect (..))

newtype IntegralImage = IntegralImage (Array (Int, Int) Int64)
    deriving (Show, Eq)
type Pixel = Int64

-- | Computes an 'IntegralImage' using a transformation function on each pixel.
integralImage :: G.GreyImage -> (Int64 -> Int64) -> IntegralImage
integralImage image f =
    IntegralImage $ integral
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

    Size w h = I.getSize image
    value x y = f $ int64 $ image `I.getPixel` Point x y
{-# INLINABLE integralImage #-}

instance I.Image IntegralImage Int64 where
    fromList size xs =
        IntegralImage $ listArray (imageShape size) xs
    {-# INLINE fromList #-}
    
    getSize (IntegralImage image) =
        let (h, w) = snd $ bounds $ image
        in Size (w + 1) (h + 1)
    {-# INLINE getSize #-}

    IntegralImage image `getPixel` Point x y = 
       image ! (y, x)
    {-# INLINE getPixel #-}
       
-- | Computes the sum of values inside a rectangle using an 'IntegralImage'.
sumRectangle integral (Rect x y w h) =
    -- a ------- b
    -- -         -
    -- -    S    -
    -- -         -
    -- c ------- d
    let a = integral `I.getPixel` Point x y
        b = integral `I.getPixel` Point (x+w) y
        c = integral `I.getPixel` Point x (y+h)
        d = integral `I.getPixel` Point (x+w) (y+h)
    in d + a - b - c
{-# INLINE sumRectangle #-}

-- | Returns the shape of an image of the given size.
imageShape :: Size -> ((Int, Int), (Int, Int))
imageShape (Size w h) = ((0, 0), (h-1, w-1))
{-# INLINE imageShape #-}

int64 :: Integral a => a -> Int64
int64 = fromIntegral