{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBImage.Conversion (
    ) where

import Data.Convertible (Convertible (..))

import Vision.Image (fromFunction, getSize, getPixel)
import Vision.Image.RGBImage.Base (RGBImage, Pixel (..))
import qualified Vision.Image.GreyImage.Base as G
import qualified Vision.Image.RGBAImage.Base as R

-- | Converts a greyscale image to RGB.
instance Convertible G.GreyImage RGBImage where
    safeConvert image =
        return $! fromFunction (getSize image) (pixFromGrey . getPixel image)
      where
        pixFromGrey pix = Pixel pix pix pix
        {-# INLINE pixFromGrey #-}
    {-# INLINE safeConvert #-}
    
-- | Converts a RGBA image to RGB.
instance Convertible R.RGBAImage RGBImage where
    safeConvert image =
        return $! fromFunction (getSize image) (pixFromRGBA . getPixel image)
      where
        pixFromRGBA (R.Pixel r g b a) =
            let a' = int a
                withAlpha c = int c * a' `quot` 255
            in Pixel (withAlpha r) (withAlpha g) (withAlpha b)
        {-# INLINE pixFromRGBA #-}
    {-# INLINE safeConvert #-}