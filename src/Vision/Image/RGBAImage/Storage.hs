{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBImage.Storage (
    ) where

import Data.Word

import Data.Array.Repa (
      Z (..), (:.) (..), (!), unsafeIndex, extent, delay, computeS
    )
    
import qualified Data.Array.Repa.IO.DevIL as IL

import Data.Convertible (convert)

import qualified Vision.Image as I
import Vision.Image.RGBAImage.Base
import Vision.Image.RGBAImage.Conversion
import Vision.Image.GreyImage.Base (GreyImage (..))
import Vision.Image.RGBImage.Base (RGBImage (..))

import Vision.Primitive (Point (..), Size (..))

instance I.StorableImage RGBAImage Pixel where
    load path =
        IL.runIL $ fromILImage `fmap` IL.readImage path
    {-# INLINE load #-}
        
    save path (RGBAImage image) = 
        IL.runIL $ IL.writeImage path (IL.RGBA $ computeS image)
    {-# INLINE save #-}

-- | Converts an image from its DevIL representation to a 'RGBAImage'.
fromILImage :: IL.Image -> RGBAImage
fromILImage (IL.RGB i)  = convert $ R.RGBImage $ delay i
fromILImage (IL.RGBA i) = RGBAImage $ delay i
fromILImage (IL.Grey i) = convert $ GreyImage $ delay i
{-# INLINE fromILImage #-}