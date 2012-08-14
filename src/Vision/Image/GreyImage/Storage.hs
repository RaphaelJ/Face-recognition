{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.GreyImage.Storage (
    ) where

import Data.Array.Repa (delay, computeS)
import qualified Data.Array.Repa.IO.DevIL as IL

import Data.Convertible (Convertible (..), convert)

import Vision.Image.GreyImage.Base (GreyImage (..))
import Vision.Image.GreyImage.Conversion ()
import Vision.Image.RGBImage.Base as RGB
import Vision.Image.RGBAImage.Base as RGBA
    
-- | Converts an DevIL image to a grey image.
instance Convertible IL.Image GreyImage where
    safeConvert (IL.RGB i) = return $! convert $ RGB.RGBImage $ delay i
    safeConvert (IL.RGBA i) = return $! convert $ RGBA.RGBAImage $ delay i
    safeConvert (IL.Grey i) = return $! GreyImage $ delay i
    {-# INLINE safeConvert #-}
    
-- | Converts an grey image to its DevIL representation.
instance Convertible GreyImage IL.Image where
    safeConvert (GreyImage image) =
        return $! IL.Grey $ computeS image
    {-# INLINE safeConvert #-}