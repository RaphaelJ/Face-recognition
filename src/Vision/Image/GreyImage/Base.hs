{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.GreyImage.Base (
    -- * Types & constructors
      GreyImage (..), GreyPixel
    -- * Functions
    , imageShape
    ) where

import Data.Word

import Data.Array.Repa (
      Array, D, DIM2, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed
    , computeUnboxedS, extent, fromFunction, delay
    )

import qualified Vision.Image.IImage as I
import Vision.Primitive (Point (..), DPoint, Size (..))

-- | Greyscale image (y :. x).
newtype GreyImage = GreyImage (Array D DIM2 Word8)
type GreyPixel = Word8

instance I.Image GreyImage Word8 Word8 where
    fromList size xs =
        GreyImage $ delay $ fromListUnboxed (imageShape size) xs
    {-# INLINE fromList #-}

    fromFunction size f =
        GreyImage $ fromFunction (imageShape size) $ \(Z :. y :. x) ->
            f $ Point x y
    {-# INLINE fromFunction #-}

    getSize (GreyImage image) =
        let (Z :. h :. w) = extent image
        in Size w h
    {-# INLINE getSize #-}

    GreyImage image `getPixel` Point x y =
        image ! (Z :. y :. x)
    {-# INLINE getPixel #-}

    GreyImage image `unsafeGetPixel` Point x y =
        image `unsafeIndex` (Z :. y :. x)
    {-# INLINE unsafeGetPixel #-}  

    force (GreyImage image) = 
        GreyImage $ delay $ computeUnboxedS image
    {-# INLINE force #-}

instance I.Pixel Word8 Word8 where
    pixToValues pix = [pix]
    {-# INLINE pixToValues #-}

    valuesToPix ~(pix:_) = pix
    {-# INLINE valuesToPix #-}

    pix `pixApply` f = f pix
    {-# INLINE pixApply #-}

{-# SPECIALIZE I.unsafeBilinearInterpol :: GreyImage -> DPoint -> GreyPixel #-}
{-# SPECIALIZE I.resize :: GreyImage -> Size -> GreyImage #-}
{-# SPECIALIZE I.horizontalFlip :: GreyImage -> GreyImage #-}

-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM2
imageShape (Size w h) = Z :. h :. w
{-# INLINE imageShape #-}