{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBImage.Conversion (
    ) where

import Data.Convertible (Convertible)

import Vision.Image (fromFunction, getSize, getPixel)
import Vision.Image.RGBImage.Base (RGBImage, Pixel)
import Vision.Image.GreyImage.Base (GreyImage)

-- | Converts a greyscale image to RGB.
instance Convertible (GreyImage) (RGBImage) where
    safeConvert image =
        return $! fromFunction (getSize image) (pixToRGBA . getPixel image)
      where
        pixFromGrey pix = Pixel pix pix pix
        {-# INLINE pixToRGBA #-}
    {-# INLINE safeConvert #-}