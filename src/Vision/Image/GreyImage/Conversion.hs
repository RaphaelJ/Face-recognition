{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.GreyImage.Conversion (
    ) where

import Data.Word

import Data.Convertible (Convertible (..))

import qualified Data.Array.Repa.IO.DevIL as IL

import Vision.Image.IImage (fromFunction, getSize, getPixel)
import Vision.Image.GreyImage.Base (GreyImage (..))
import Vision.Image.RGBImage.Base as RGB
import Vision.Image.RGBAImage.Base as RGBA

-- | Converts a RGBA image to greyscale.
instance Convertible RGBA.RGBAImage GreyImage where
    safeConvert image =
        return $! fromFunction (getSize image) (pixFromRGBA . getPixel image)
      where
        pixFromRGBA (RGBA.RGBAPixel r g b a) =
            word8 $ rgbToGrey r g b * int a `quot` 255
        {-# INLINE pixFromRGBA #-}
    {-# INLINE safeConvert #-}

-- | Converts a RGB image to greyscale.
instance Convertible RGB.RGBImage GreyImage where
    safeConvert image =
        return $! fromFunction (getSize image) (pixFromRGB . getPixel image)
      where
        pixFromRGB (RGB.RGBPixel r g b) =
            word8 $ rgbToGrey r g b
        {-# INLINE pixFromRGB #-}
    {-# INLINE safeConvert #-}
    
-- | Converts the colors to greyscale using the human eye perception of colors.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Int
rgbToGrey r g b = (int r * 30 + int g * 59 + int b * 11) `quot` 100
{-# INLINE rgbToGrey #-}

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral