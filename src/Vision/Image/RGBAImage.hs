{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Image.RGBAImage (
    -- * Types & constructors
      RGBAImage (..), Pixel (..)
    -- * Functions
    , imageShape
) where

import Data.Word

import Data.Array.Repa (
      Array, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromList, fromFunction
    , extent
    )
import qualified Data.Array.Repa.IO.DevIL as IL

import qualified Vision.Image as I
import Vision.Primitive (Point (..), Size (..))

-- | RGBA image (y :. x :. channel).
newtype RGBAImage = RGBAImage (Array DIM3 Word8)
    deriving (Show, Eq)
data Pixel = Pixel {
    red :: !Word8, green :: !Word8, blue :: !Word8, alpha :: !Word8
    } deriving (Show, Read, Eq)

instance I.Image RGBAImage Pixel where
    fromList size xs =
        RGBAImage $ fromList (imageShape size) $ 
            concat [ [r, b, b, a] | Pixel r g b a <- xs ]
    
    fromFunction size f =
        RGBAImage $ fromFunction (imageShape size) $ \(Z :. y :. x :. c) ->
            let point = Point x y
            in case c of
                 0 -> red $ f point
                 1 -> green $ f point
                 2 -> blue $ f point
                 3 -> alpha $ f point

    getSize (RGBAImage image) =
        let (Z :. h :. w :. _) = extent image
        in Size w h

    RGBAImage image `getPixel` Point x y =
        let coords = Z :. y :. x
        in Pixel {
              red = image ! (coords :. 0)
            , green = image ! (coords :. 1)
            , blue = image ! (coords :. 2)
            , alpha = image ! (coords :. 3)
        }

    RGBAImage image `unsafeGetPixel` Point x y =
        let coords = Z :. y :. x
        in Pixel {
              red = image `unsafeIndex` (coords :. 0)
            , green = image `unsafeIndex` (coords :. 1)
            , blue = image `unsafeIndex` (coords :. 2)
            , alpha = image `unsafeIndex` (coords :. 3)
        }

instance I.StorableImage RGBAImage Pixel  where
    load path = RGBAImage `fmap` (IL.runIL $ IL.readImage path)
    save path (RGBAImage image) =  IL.runIL $ IL.writeImage path image

-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 4