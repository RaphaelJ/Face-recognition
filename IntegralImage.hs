module IntegralImage (
      IntegralImage
    , computeIntegralImage, getValue, imageSize
) where
    
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int
import Data.List
import Data.Word

import Primitives
import GreyImage (GreyImage, getPixel)

type IntegralImage = UArray Point Int64

computeIntegralImage :: GreyImage -> (Int64 -> Int64) -> IntegralImage
computeIntegralImage image f =
    runSTUArray computeIntegralImage'
  where
    computeIntegralImage' = do
        integral <- newArray_ (bounds image) :: ST s (STUArray s Point Int64)

        forM_ (assocs image) $ \(coord@(Point x y), pix) -> do
            let pix' = f $ fromIntegral pix
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

getValue :: IntegralImage -> Word16 -> Word16 -> Int64
getValue image x y | x == 0 || y == 0 = 0
                   | otherwise        = image ! Point (x-1) (y-1)

-- | Give the original image's size
imageSize :: IntegralImage -> Size
imageSize image =
    let Point w h = snd $ bounds $ image
    in Size (w + 1) (h + 1)