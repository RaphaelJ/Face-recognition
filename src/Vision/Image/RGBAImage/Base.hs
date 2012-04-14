{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBAImage.Base (
    -- * Types & constructors
      RGBAImage (..), Pixel (..)
    -- * Functions
    , imageShape
    ) where

import Data.Word

import Data.Array.Repa (
      Array, D, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed
    , fromFunction, extent, delay
    )

import qualified Vision.Image as I
import Vision.Primitive (Point (..), Size (..))

-- | RGBA image (y :. x :. channel).
newtype RGBAImage = RGBAImage (Array D DIM3 Word8)

data Pixel = Pixel {
      red ::   {-# UNPACK #-} !Word8
    , green :: {-# UNPACK #-} !Word8
    , blue ::  {-# UNPACK #-} !Word8
    , alpha :: {-# UNPACK #-} !Word8
    } deriving (Show, Read, Eq)

instance I.Image RGBAImage Pixel where
    fromList size xs =
        RGBAImage $ delay $ fromListUnboxed (imageShape size) $ 
            concat [ [r, g, b, a] | Pixel r g b a <- xs ]
    {-# INLINE fromList #-}
    
    fromFunction size f =
        RGBAImage $ fromFunction (imageShape size) $ \(Z :. y :. x :. c) ->
            let point = Point x y
            in case c of
                 0 -> red $ f point
                 1 -> green $ f point
                 2 -> blue $ f point
                 3 -> alpha $ f point
    {-# INLINE fromFunction #-}

    getSize (RGBAImage image) =
        let (Z :. h :. w :. _) = extent image
        in Size w h
    {-# INLINE getSize #-}

    RGBImage image `getPixel` Point x y =
        let coords = Z :. y :. x
        in Pixel {
              red = image ! (coords :. 0)
            , green = image ! (coords :. 1)
            , blue = image ! (coords :. 2)
            , alpha = image ! (coords :. 3)
        }
    {-# INLINE getPixel #-}

    RGBImage image `unsafeGetPixel` Point x y =
        let coords = Z :. y :. x
        in Pixel {
              red = image `unsafeIndex` (coords :. 0)
            , green = image `unsafeIndex` (coords :. 1)
            , blue = image `unsafeIndex` (coords :. 2)
            , alpha = image `unsafeIndex` (coords :. 3)
        }
    {-# INLINE unsafeGetPixel #-}
    
-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 4
{-# INLINE imageShape #-}