{-# LANGUAGE TypeOperators #-}

module Vision.Image.RGBAImage (
    -- * Types & constructors
      RGBAImage, Pixel (..), create
    -- * Filesystem images manipulations
    , load, save
    -- * Functions
    , getPixel, getSize, resize, imageShape
) where

import Data.Word

import Data.Array.Repa (
      Array, DIM3, Z (..), (:.) (..), (!), unsafeIndex, fromList, extent
    )
import qualified Data.Array.Repa.IO.DevIL as IL

import Vision.Primitive (Point (..), Size (..))

-- | RGBA image (y :. x :. channel).
type RGBAImage = Array DIM3 Word8
data Pixel = Pixel {
    red :: !Word8, green :: !Word8, blue :: !Word8, alpha :: !Word8
    } deriving (Show, Read)

-- | Creates a new image from a list of pixels.
create :: Size -> [Word8] -> RGBAImage
create size xs = fromList (imageShape size) xs

-- | Loads an image at system path and detects image\'s type.
load :: FilePath -> IO RGBAImage
load = IL.runIL . IL.readImage

-- | Saves an image.
save :: FilePath -> RGBAImage -> IO ()
save path image =
    IL.runIL $ IL.writeImage path image
    
-- | Gets a pixel from the image.
getPixel :: RGBAImage -> Point -> Pixel
getPixel image (Point x y) =
    Pixel {
          red = image ! (Z :. y :. x :. 0)
        , green = image ! (Z :. y :. x :. 1)
        , blue = image ! (Z :. y :. x :. 2)
        , alpha = image ! (Z :. y :. x :. 3)
    }
{-# INLINE getPixel #-}

-- | Gets image\'s size.
getSize :: RGBAImage -> Size
getSize image = 
    let (Z :. h :. w :. _) = extent image
    in Size w h
{-# INLINE getSize #-}

-- | Resizes an image using the nearest-neighbor interpolation.
resize :: RGBAImage -> Size -> RGBAImage
resize image size'@(Size w' h') =
    create size' [ v |
          y' <- [0..h'-1], x' <- [0..w'-1], c <- [0..3]
        , let !y = y' * h `quot` h'
        , let !x = x' * w `quot` w'
        , let !v = image `unsafeIndex` (Z :. y :. x :. c)
    ]
  where
    Size w h = getSize image

-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM3
imageShape (Size w h) = Z :. h :. w :. 4