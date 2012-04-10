{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.GreyImage (
    -- * Types & constructors
      GreyImage (..), Pixel
    -- * Functions
    , fromRGB, toRGB, imageShape
    ) where

import Data.Word

import Data.Array.Repa (
      Array, DIM2, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed, extent
    , fromFunction, delay
    )

import qualified Vision.Image as I
import qualified Vision.Image.RGBImage as R
import Vision.Primitive (
    Point (..), Size (..), Rect (..)
    )

-- | Greyscale image (y :. x).
newtype GreyImage = GreyImage (Array DIM2 Word8)
    deriving (Show, Eq)
type Pixel = Word8

instance I.Image GreyImage Word8 where
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
        
    GreyImage image `getPixel` Point x y =
        image ! (Z :. y :. x)
    GreyImage image `unsafeGetPixel` Point x y =
        image `unsafeIndex` (Z :. y :. x)

instance I.StorableImage GreyImage Word8 where
    load path = do
        image <- I.load path
        return $ fromRGBA image
        
    save path image = I.save path $ toRGBA image

fromRGB :: R.RGBImage -> GreyImage
fromRGB image =
    I.fromFunction (I.getSize image) (pixFromRGBA . I.getPixel image)

toRGB :: GreyImage -> R.RGBImage
toRGB image =
    I.fromFunction (I.getSize image) (pixToRGBA . I.getPixel image)

pixFromRGB (R.Pixel r g b a) =
    let r' = int r * 30
        g' = int g * 59
        b' = int b * 11
    in word8 $ (r' + g' + b') `quot` 100

pixToRGBA pix = R.Pixel pix pix pix 255

-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM2
imageShape (Size w h) = Z :. h :. w
{-# INLINE imageShape #-}

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral