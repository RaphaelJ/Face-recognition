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

bilinearInterpol :: (Pixel p a, Image i p a, Integral a) 
                 => i -> Point Double Double -> p
i `bilinearInterpol` Point x y = 
    
  where
    (x1, y1) = (truncate x, truncate y)
    (x2, y2) = (x1 + 1, y1 + 1)
    [d_x1, d_y1, d_x2, d_y2] = map fromIntegral [x1, y1, x2, y2]
    a = 
        (d_x2 - x) * (d_y2 - y)
    b = (x - d_x1) * (d_y2 - y)
    c = (d_x2 - x) * (y - d_y1)
    d = (x - d_x1) * (y - d_y1)
    pixVal pond = realToFrac 
    

        double d_x1 = (double) x1
         , d_y1 = (double) y1
         , d_x2 = (double) x2
         , d_y2 = (double) y2;
    
    return a * (d_x2 - x) * (d_y2 - y) + b * (x - d_x1) * (d_y2 - y)
         + c * (d_x2 - x) * (y - d_y1) + d * (x - d_x1) * (y - d_y1);
}

inline double bilinear_interpol(
    const s_partial_der& der, const double x, const double y
)
{
    int x1 = (int) x
      , y1 = (int) y;
    int x2 = x1 + 1
      , y2 = y1 + 1;
    
    
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