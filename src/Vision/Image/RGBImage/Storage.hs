{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBImage.Conversion (
s    -- * Types & constructors
      RGBImage (..), Pixel (..)
    -- * Functions
    , fromGrey, imageShape
    ) where

import Data.Word

import Data.Array.Repa (
      Array, D, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed
    , extent, delay, computeS
    )
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as IL

import qualified Vision.Image as I
import qualified Vision.Image.RGBImage.Bases

import Vision.Primitive (Point (..), Size (..))

instance I.StorableImage RGBImage Pixel where
    load path =
        IL.runIL $ fromILImage `fmap` IL.readImage path
    {-# INLINE load #-}
        
    save path (RGBImage image) = 
        IL.runIL $ IL.writeImage path (IL.RGB $ computeS image)
    {-# INLINE save #-}

-- | Converts a greyscale image to RGB.
fromGrey :: G.GreyImage -> RGBImage
fromGrey image =
    I.fromFunction (I.getSize image) (pixToRGBA . I.getPixel image)
  where
    pixFromGrey pix = R.Pixel pix pix pix
    {-# INLINE pixToRGBA #-}
{-# INLINE fromGrey #-}

-- | Converts an image from its DevIL representation to a 'RGBImage'.
fromILImage :: IL.Image -> RGBImage
fromILImage (IL.RGB i)  = RGBImage $ delay i
fromILImage (IL.RGBA i) = 
    I.fromFunction (Size w h) $ \(Point x y) ->
        let coords = Z :. y :. x
        in Pixel {
              red = i ! (coords :. 0)
            , green = i ! (coords :. 1)
            , blue = i ! (coords :. 2)
        }
  where
    (Z :. h :. w :. _) = extent i
fromILImage (IL.Grey i) = fromGrey i
{-# INLINE fromILImage #-}
    
-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 3
{-# INLINE imageShape #-}