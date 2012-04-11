{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBImage.Base (
    -- * Types & constructors
      RGBImage (..), Pixel (..)
    -- * Functions
    , imageShape
    ) where

import Data.Word

import Data.Array.Repa (
      Array, D, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed
    , extent, delay, computeS
    )
import qualified Data.Array.Repa as R

import qualified Vision.Image as I
import Vision.Primitive (Point (..), Size (..))

-- | RGB image (y :. x :. channel).
newtype RGBImage = RGBImage (Array D DIM3 Word8)

data Pixel = Pixel {
      red ::   {-# UNPACK #-} !Word8
    , green :: {-# UNPACK #-} !Word8
    , blue ::  {-# UNPACK #-} !Word8
    } deriving (Show, Read, Eq)

instance I.Image RGBImage Pixel where
    fromList size xs =
        RGBImage $ delay $ fromListUnboxed (imageShape size) $ 
            concat [ [r, b, b] | Pixel r g b <- xs ]
    {-# INLINE fromList #-}
    
    fromFunction size f =
        RGBImage $ R.fromFunction (imageShape size) $ \(Z :. y :. x :. c) ->
            let point = Point x y
            in case c of
                 0 -> red $ f point
                 1 -> green $ f point
                 2 -> blue $ f point
    {-# INLINE fromFunction #-}

    getSize (RGBImage image) =
        let (Z :. h :. w :. _) = extent image
        in Size w h
    {-# INLINE getSize #-}

    RGBImage image `getPixel` Point x y =
        let coords = Z :. y :. x
        in Pixel {
              red = image ! (coords :. 0)
            , green = image ! (coords :. 1)
            , blue = image ! (coords :. 2)
        }
    {-# INLINE getPixel #-}

    RGBImage image `unsafeGetPixel` Point x y =
        let coords = Z :. y :. x
        in Pixel {
              red = image `unsafeIndex` (coords :. 0)
            , green = image `unsafeIndex` (coords :. 1)
            , blue = image `unsafeIndex` (coords :. 2)
        }
    {-# INLINE unsafeGetPixel #-}
    
-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 3
{-# INLINE imageShape #-}