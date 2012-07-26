{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Vision.Image (
      module Vision.Image.IImage
    , module Vision.Image.GreyImage
    , module Vision.Image.RGBAImage
    , module Vision.Image.RGBImage
    
    -- * Misc images tranformations
    , resize, drawRectangle, horizontalFlip
    ) where
        
import Debug.Trace
    
import Data.Convertible (safeConvert, convert)

import Vision.Primitive
import Vision.Image.IImage
import Vision.Image.GreyImage
import Vision.Image.RGBAImage
import Vision.Image.RGBImage
    
-- | Uses a bilinear interpolation to find the value of the pixel at the
-- floating point coordinates.
-- Estimates the value of P using A, B, C and D :
-- q ------ r
-- -        -
-- -  P     -
-- -        -
-- s ------ t
bilinearInterpol, unsafeBilinearInterpol 
    :: (Pixel p a, Image i p a, Integral a) 
    => i -> (Double, Double) -> p
i `bilinearInterpol` (x, y) = 
    if x >= 0 && y >= 0 && x < fromIntegral w && y < fromIntegral h
       then i `unsafeBilinearInterpol` (x, y)
       else error "Invalid index"
  where
    Size w h = getSize i

-- | Uses a bilinear interpolation whithout checking bounds.
i `unsafeBilinearInterpol` (x, y) =
    valuesToPix $ interpolateChannels qs rs ss ts
  where
    (x1, y1) = (truncate x, truncate y)
    (x2, y2) = (x1 + 1, y1 + 1)
    (d_x1, d_y1, d_x2, d_y2) =
        (fromIntegral x1, fromIntegral y1, fromIntegral x2, fromIntegral y2)
    interpolate q r s t =
        let (q', r') = (fromIntegral q, fromIntegral r)
            (s', t') = (fromIntegral s, fromIntegral t)
        in round $
              q' * (d_x2 - x) * (d_y2 - y) + r' * (x - d_x1) * (d_y2 - y) 
            + s' * (d_x2 - x) * (y - d_y1) + t' * (x - d_x1) * (y - d_y1)
    interpolateChannels []      []      []      []      = []
    interpolateChannels (q:qs') (r:rs') (s:ss') (t:ts') =
        interpolate q r s t : interpolateChannels qs' rs' ss' ts'
    
    qs = pixToValues $ i `unsafeGetPixel` Point x1 y1
    rs = pixToValues $ i `unsafeGetPixel` Point x2 y1
    ss = pixToValues $ i `unsafeGetPixel` Point x1 y2
    ts = pixToValues $ i `unsafeGetPixel` Point x2 y2
{-# INLINE bilinearInterpol #-}
    
-- | Resizes the 'Image' using the nearest-neighbor interpolation.
resize :: Image i p a => i -> Size -> i
resize image size'@(Size w' h') =
    fromFunction size' $ \(Point x' y') ->
        let x = x' * w `quot` w'
            y = y' * h `quot` h'
        in image `unsafeGetPixel` Point x y
  where
    Size w h = getSize image
{-# INLINABLE resize #-}

-- | Draws a rectangle inside the 'IImage' using two transformation functions.
drawRectangle :: Image i p a => i
              -> (p -> p) -- ^ Border transformation
              -> (p -> p) -- ^ Background transformation
              -> Rect -> i
drawRectangle image back border (Rect rx ry rw rh) =
    fromFunction (getSize image) $ \pt -> draw pt (image `unsafeGetPixel` pt)
  where
    draw pt pix
        | inBackground pt = back pix
        | inBorder pt     = border pix
        | otherwise       = pix
      
    inBackground (Point x y) =
        x > rx && y > ry && x < rx' && y < ry'
    
    inBorder (Point x y) =
        x >= rx && y >= ry && x <= rx' && y <= ry'

    rx' = rx + rw - 1
    ry' = ry + rh - 1
{-# INLINABLE drawRectangle #-}

-- | Reverses the image horizontally.
horizontalFlip :: Image i p a => i -> i
horizontalFlip image =
    fromFunction (getSize image) $ \(Point x' y) ->
        let x = maxX - x'
        in image `unsafeGetPixel` Point x y
  where
    Size w h = getSize image
    maxX = w - 1
{-# INLINABLE horizontalFlip #-}