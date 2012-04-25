{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBAImage.Conversion (
    ) where

import Data.Convertible (Convertible (..))

import Vision.Image (fromFunction, getSize, getPixel)
import Vision.Image.RGBAImage.Base (RGBAImage, Pixel (..))
import qualified Vision.Image.GreyImage.Base as G
import qualified Vision.Image.RGBImage.Base as R

-- | Converts a greyscale image to RGBA.
instance Convertible G.GreyImage RGBAImage where
    safeConvert image =
        return $! fromFunction (getSize image) (pixFromGrey . getPixel image)
      where
        pixFromGrey pix = Pixel pix pix pix 255
        {-# INLINE pixFromGrey #-}
    {-# INLINE safeConvert #-}
    
-- | Converts a RGB image to RGBA.
instance Convertible R.RGBImage RGBAImage where
    safeConvert image =
        return $! fromFunction (getSize image) (pixFromRGB . getPixel image)
      where
        pixFromRGB (R.Pixel r g b) = Pixel r g b 255
        {-# INLINE pixFromRGB #-}
    {-# INLINE safeConvert #-}