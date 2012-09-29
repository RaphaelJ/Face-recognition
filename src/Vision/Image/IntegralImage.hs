{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.IntegralImage (
    -- * Type
      IntegralImage (..), IIPixel
    -- * Functions 
    , integralImage, sumRectangle, imageShape, originalSize
    ) where

import Control.Monad
import Control.Monad.ST (ST)

import Data.Array.IArray ((!), listArray, bounds)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.STRef (newSTRef, readSTRef, modifySTRef)

import Data.Int

import qualified Vision.Image.IImage as I
import qualified Vision.Image.GreyImage as G
import Vision.Primitive (Point (..), Size (..), Rect (..))

newtype IntegralImage = IntegralImage (UArray (Int, Int) Int64)
    deriving (Show, Eq)
type IIPixel = Int64

-- | Computes an 'IntegralImage' using a transformation function on each pixel.
integralImage :: G.GreyImage -> (Int64 -> Int64) -> IntegralImage
integralImage image f =
    IntegralImage (runSTUArray integral)
  where
    integral :: ST s (STUArray s (Int, Int) Int64)
    integral = do
        arr <- newArray ((0, 0), (h, w)) 0

        forM_ [1..h] $ \y -> do
            colSumRef <- newSTRef 0
            forM_ [1..w] $ \x -> do
                let pix = value (x-1) (y-1)
                top <- readArray arr (y-1, x)

                modifySTRef colSumRef (+ pix)
                colSum <- readSTRef colSumRef
                writeArray arr (y, x) (colSum + top)

        return arr

--  This pure implementation is not as efficient and uses a lot of memory.
--
--     uintegral = listArray (bounds integral) (elems integral)
--       
--     integral :: Array (Int, Int) Int64
--     integral = array ((0, 0), (h, w)) (topValues ++ leftValues ++ values)
-- 
--     -- Initializes the first row and the first column to zero
--     topValues = map (\x -> ((0, x), 0)) [0..w]
--     leftValues = map (\y -> ((y, 0), 0)) [0..h]
-- 
--     values = [ ((y, x), pix + left + top - topLeft) |
--           y <- [1..h], x <- [1..w]
--         , let pix = value (x-1) (y-1)
--         , let topLeft = integral ! (y-1, x-1)
--         , let top = integral ! (y-1, x)
--         , let left = integral ! (y, x-1)
--         ]
-- 
    Size w h = I.getSize image
    value x y = f $ int64 $ image `I.getPixel` Point x y
{-# INLINE integralImage #-} -- Inline the transformation function

instance I.Image IntegralImage Int64 Int64 where
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

instance I.Pixel Int64 Int64 where
    pixToValues pix = [pix]
    {-# INLINE pixToValues #-}

    valuesToPix ~[pix] = pix
    {-# INLINE valuesToPix #-}

    pix `pixApply` f = f pix
    {-# INLINE pixApply #-}
       
-- | Computes the sum of values inside a rectangle using an 'IntegralImage'.
sumRectangle :: IntegralImage -> Rect -> Int64
sumRectangle ii (Rect x y w h) =
    -- a ------- b
    -- -         -
    -- -    S    -
    -- -         -
    -- c ------- d
    let a = ii `I.unsafeGetPixel` Point x y
        b = ii `I.unsafeGetPixel` Point (x+w) y
        c = ii `I.unsafeGetPixel` Point x (y+h)
        d = ii `I.unsafeGetPixel` Point (x+w) (y+h)
    in d + a - b - c
{-# INLINE sumRectangle #-}

-- | Returns the shape of an image of the given size.
imageShape :: Size -> ((Int, Int), (Int, Int))
imageShape (Size w h) = ((0, 0), (h-1, w-1))
{-# INLINE imageShape #-}

-- | Returns the size of the original image.
originalSize :: IntegralImage -> Size
originalSize ii = 
    Size (iiWidth - 1) (iiHeight - 1)
  where
    Size iiWidth iiHeight = I.getSize ii
{-# INLINE originalSize #-}

int64 :: Integral a => a -> Int64
int64 = fromIntegral