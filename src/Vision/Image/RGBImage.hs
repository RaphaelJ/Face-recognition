{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBImage (
    -- * Types & constructors
      RGBImage (..), Pixel (..)
    -- * Functions
    , imageShape
) where

import Data.Word

import Data.Array.Repa (
      Array, D, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromListUnboxed,
    , fromFunction, extent, delay
    )
import qualified Data.Array.Repa.IO.DevIL as IL

import qualified Vision.Image as I
import Vision.Primitive (Point (..), Size (..))

-- | RGB image (y :. x :. channel).
newtype RGBImage = RGBImage (Array D DIM3 Word8)
    deriving (Show, Eq)
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
        RGBImage $ fromFunction (imageShape size) $ \(Z :. y :. x :. c) ->
            let point = Point x y
            in case c of
                 0 -> red $ f point
                 1 -> green $ f point
                 2 -> blue $ f point

    getSize (RGBImage image) =
        let (Z :. h :. w :. _) = extent image
        in Size w h

    RGBImage image `getPixel` !(Point x y) =
        let coords = Z :. y :. x
        in Pixel {
              red = image ! (coords :. 0)
            , green = image ! (coords :. 1)
            , blue = image ! (coords :. 2)
        }

    RGBImage image `unsafeGetPixel` !(Point x y) =
        let coords = Z :. y :. x
        in Pixel {
              red = image `unsafeIndex` (coords :. 0)
            , green = image `unsafeIndex` (coords :. 1)
            , blue = image `unsafeIndex` (coords :. 2)
        }

instance I.StorableImage (RGBImage D) Pixel  where
    load path = IL.runIL $ fromILImage `fmap` IL.readImage path
        
    save path (RGBImage image) = 
        IL.runIL $ IL.writeImage path image


fromILImage :: IL.Image -> RGBImage
fromILImage (IL.RGB i)  = RGBImage $ delay i
fromILImage (IL.RGBA i) = 
    fromFunction (Size w h) $ \!(Point x y) ->
        let coords = Z :. y :. x
        in Pixel {
              red = image ! (coords :. 0)
            , green = image ! (coords :. 1)
            , blue = image ! (coords :. 2)
        }
  where
    (Z :. h :. w :. _) = extent i
fromILImage (IL.Grey i) = RGBImage $ delay i
    fromFunction (Size w h) $ \!(Point x y) ->
        let val = i ! (Z :. y :. x)
        in Pixel val val val
  where
    (Z :. h :. w) = extent i
    
-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 3