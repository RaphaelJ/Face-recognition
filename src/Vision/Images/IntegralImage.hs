module Vision.Images.IntegralImage (
    -- * Type
      IntegralImage
    -- * Functions
    , computeIntegralImage, getValue, imageSize
    ) where
    
import Control.Monad
import Control.Monad.ST
import Data.Array.ST (STUArray, newArray_, runSTUArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, (!),  assocs, bounds)
import Data.Int

import Vision.Images.GreyImage (GreyImage, getPixel)
import Vision.Primitives (Point (..), Size (..))

type IntegralImage = UArray Point Int64

-- | Computes an 'IntegralImage' using a transformation function on each pixel.
computeIntegralImage :: GreyImage -> (Int64 -> Int64) -> IntegralImage
computeIntegralImage image f = runSTUArray $ do
    integral <- newArray_ (bounds image) :: ST s (STUArray s Point Int64)

    forM_ (assocs image) $ \(coord@(Point x y), pix) -> do
        let pix' = f $ int64 pix
        topLeft <- if x > 0 && y > 0
            then readArray integral (Point (x-1) (y-1))
            else return 0
        top <- if y > 0
            then readArray integral (Point x (y-1))
            else return 0
        left <- if x > 0
            then readArray integral (Point (x-1) y)
            else return 0

        writeArray integral coord (pix' + left + top - topLeft)

    return integral

-- | Gets the value of a point inside an 'IntegralImage'. A value with x or y
-- equals to 0 will ever be 0.
getValue :: IntegralImage -> Point -> Int64
getValue image (Point x y) | x == 0 || y == 0 = 0
                           | otherwise        = image ! Point (x-1) (y-1)

-- | Gives the original image\'s size.
imageSize :: IntegralImage -> Size
imageSize image =
    let Point w h = snd $ bounds $ image
    in Size (w + 1) (h + 1)

int64 :: Integral a => a -> Int64
int64 = fromIntegral