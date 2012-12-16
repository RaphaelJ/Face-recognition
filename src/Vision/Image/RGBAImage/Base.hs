{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBAImage.Base (
    -- * Types & constructors
      RGBAImage (..), RGBAPixel (..)
    ) where

import Data.Word

import Data.Array.Repa (
      Array, D, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed
    , computeUnboxedS, fromFunction, extent, delay
    )

import qualified Vision.Image.IImage as I
import Vision.Primitive (Point (..), DPoint, Rect, Size (..))

-- | RGBA image (y :. x :. channel).
newtype RGBAImage = RGBAImage (Array D DIM3 Word8)

data RGBAPixel = RGBAPixel {
      rgbaRed ::   {-# UNPACK #-} !Word8
    , rgbaGreen :: {-# UNPACK #-} !Word8
    , rgbaBlue ::  {-# UNPACK #-} !Word8
    , rgbaAlpha :: {-# UNPACK #-} !Word8
    } deriving (Show, Read, Eq)

instance I.Image RGBAImage RGBAPixel Word8 where
    fromList size xs =
        RGBAImage $ delay $ fromListUnboxed (imageShape size) $ 
            concat [ [r, g, b, a] | RGBAPixel r g b a <- xs ]
    {-# INLINE fromList #-}
    
    fromFunction size f =
        RGBAImage $ fromFunction (imageShape size) $ \(Z :. y :. x :. c) ->
            let point = Point x y
            in case c of
                 0  -> rgbaRed $ f point
                 1  -> rgbaGreen $ f point
                 2  -> rgbaBlue $ f point
                 ~3 -> rgbaAlpha $ f point
    {-# INLINE fromFunction #-}

    getSize (RGBAImage image) =
        let (Z :. h :. w :. _) = extent image
        in Size w h
    {-# INLINE getSize #-}

    RGBAImage image `getPixel` Point x y =
        let coords = Z :. y :. x
        in RGBAPixel {
              rgbaRed = image ! (coords :. 0)
            , rgbaGreen = image ! (coords :. 1)
            , rgbaBlue = image ! (coords :. 2)
            , rgbaAlpha = image ! (coords :. 3)
        }
    {-# INLINE getPixel #-}

    RGBAImage image `unsafeGetPixel` Point x y =
        let coords = Z :. y :. x
        in RGBAPixel {
              rgbaRed = image `unsafeIndex` (coords :. 0)
            , rgbaGreen = image `unsafeIndex` (coords :. 1)
            , rgbaBlue = image `unsafeIndex` (coords :. 2)
            , rgbaAlpha = image `unsafeIndex` (coords :. 3)
        }
    {-# INLINE unsafeGetPixel #-}

    force (RGBAImage image) = 
        RGBAImage $ delay $ computeUnboxedS image
    {-# INLINE force #-}

instance I.Pixel RGBAPixel Word8 where
    pixToValues (RGBAPixel r g b a) = [r, g, b, a]
    {-# INLINE pixToValues #-}

    valuesToPix ~(r : g : b : a : _) = RGBAPixel r g b a
    {-# INLINE valuesToPix #-}

    RGBAPixel r g b a `pixApply` f = RGBAPixel (f r) (f g) (f b) (f a) 
    {-# INLINE pixApply #-}

{-# SPECIALIZE I.unsafeBilinearInterpol :: RGBAImage -> DPoint -> RGBAPixel #-}
-- {-# SPECIALIZE I.resize :: RGBAImage -> Size -> RGBAImage #-}
{-# SPECIALIZE I.crop :: RGBAImage -> Rect -> RGBAImage #-}
{-# SPECIALIZE I.horizontalFlip :: RGBAImage -> RGBAImage #-}

-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 4
{-# INLINE imageShape #-}