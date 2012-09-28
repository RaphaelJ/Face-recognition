{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBAImage.Conversion () where

import Data.Convertible (Convertible (..))

import Vision.Image.IImage (fromFunction, getSize, unsafeGetPixel)
import Vision.Image.RGBAImage.Base (RGBAImage, RGBAPixel (..))
import qualified Vision.Image.GreyImage.Base as G
import qualified Vision.Image.RGBImage.Base as R

-- | Converts a greyscale image to RGBA.
instance Convertible G.GreyImage RGBAImage where
    safeConvert image =
        return $! fromFunction (getSize image) $
            pixFromGrey . unsafeGetPixel image
      where
        pixFromGrey pix = RGBAPixel pix pix pix 255
        {-# INLINE pixFromGrey #-}
    {-# INLINE safeConvert #-}

-- | Converts a RGB image to RGBA.
instance Convertible R.RGBImage RGBAImage where
    safeConvert image =
        return $! fromFunction (getSize image) $
            pixFromRGB . unsafeGetPixel image
      where
        pixFromRGB (R.RGBPixel r g b) = RGBAPixel r g b 255
        {-# INLINE pixFromRGB #-}
    {-# INLINE safeConvert #-}