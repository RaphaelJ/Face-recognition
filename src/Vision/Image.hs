module Vision.Image (
      module Vision.Image.IImage
    , module Vision.Image.GreyImage
    , module Vision.Image.RGBAImage
    , module Vision.Image.RGBImage
    
    -- * Misc images tranformations
    , resize, drawRectangle
    ) where

import Vision.Primitive
import Vision.Image.IImage
import Vision.Image.GreyImage
import Vision.Image.RGBAImage
import Vision.Image.RGBImage

-- Uses a bilinear interpolation to find the value of the pixel at the floating
-- point coordinates.
-- Estimates the value of P using A, B, C and D :
-- q ------ r
-- -        -
-- -  P     -
-- -        -
-- s ------ t
bilinearInterpol :: (Pixel p a, Image i p a, Integral a) 
                 => i -> Point Double -> p
i `bilinearInterpol` Point x y = 
    if x1 >= 0 && y1 >= 0 && x2 < w && y2 < h
       then valuesToPix $ interpolateChannels qs rs ss ts
       else error "Invalid index"
  where
    Size w h = getSize i
    (x1, y1) = (truncate x, truncate y)
    (x2, y2) = (x1 + 1, y1 + 1)
    (d_x1, d_y1, d_x2, d_y2) = 
        (fromIntegral x1, fromIntegral y1, fromIntegral x2, fromIntegral y2)
    interpolate q r s t =
        let q' = fromIntegral q
            r' = fromIntegral r
            s' = fromIntegral s
            t' = fromIntegral t
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
        let x = x' * ratioW 
            y = y' * ratioH
        in image `unsafeGetPixel` Point x y
  where
    Size w h = getSize image
    (ratioW, ratioH) = (w `div` w', h `div` h')
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