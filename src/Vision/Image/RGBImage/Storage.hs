{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.RGBImage.Storage () where

import Data.Array.Repa (delay, computeS)
import qualified Data.Array.Repa.IO.DevIL as IL

import Data.Convertible (Convertible (..), convert)

import Vision.Image.RGBImage.Base (RGBImage (..))
import Vision.Image.RGBImage.Conversion ()
import Vision.Image.GreyImage.Base as G
import Vision.Image.RGBAImage.Base as R
    
-- | Converts an DevIL image to a RGB image.
instance Convertible IL.Image RGBImage where
    safeConvert (IL.RGB i) = return $! RGBImage $ delay i
    safeConvert (IL.RGBA i) = return $! convert $ R.RGBAImage $ delay i
    safeConvert (IL.Grey i) = return $! convert $ G.GreyImage $ delay i
    safeConvert _ = error "Unsupported format"
    {-# INLINE safeConvert #-}
    
-- | Converts an RGB image to its DevIL representation.
instance Convertible RGBImage IL.Image where
    safeConvert (RGBImage image) =
        return $! IL.RGB $ computeS image
    {-# INLINE safeConvert #-}