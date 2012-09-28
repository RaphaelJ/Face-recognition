{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBAImage.Storage () where

import Data.Array.Repa (delay, computeS)
import qualified Data.Array.Repa.IO.DevIL as IL

import Data.Convertible (Convertible (..), convert)

import Vision.Image.RGBAImage.Base (RGBAImage (..))
import Vision.Image.RGBAImage.Conversion ()
import Vision.Image.GreyImage.Base as G
import Vision.Image.RGBImage.Base as R
    
-- | Converts an DevIL image to a RGBA image.
instance Convertible IL.Image RGBAImage where
    safeConvert (IL.RGB i) = return $! convert $ R.RGBImage $ delay i
    safeConvert (IL.RGBA i) = return $! RGBAImage $ delay i
    safeConvert (IL.Grey i) = return $! convert $ G.GreyImage $ delay i
    safeConvert _ = error "Unsupported format"
    {-# INLINE safeConvert #-}
    
-- | Converts an RGBA image to its DevIL representation.
instance Convertible RGBAImage IL.Image where
    safeConvert (RGBAImage image) =
        return $! IL.RGBA $ computeS image
    {-# INLINE safeConvert #-}