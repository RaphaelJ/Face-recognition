module Vision.Image.GreyImage (
    -- * Types & constructors
      GreyImage, Pixel (..), create
    -- * Filesystem images manipulations
    , load, save
    -- * Functions
    , getPixel, getSize, resize,{- drawRectangle,-} imageShape
    -- * Transforms between greyscale and RGBA images
    , fromRGBA, toRGBA
    ) where

import Data.Word

import Data.Array.Repa (
      Array, DIM2, Z (..), (:.) (..), (!), unsafeIndex, fromList, extent, (//)
    )

import qualified Vision.Image.RGBAImage as R
import Vision.Primitive (
    Point (..), Size (..), Rect (..), sizeRange
    )

-- | Greyscale image (y :. x).
type GreyImage = Array DIM2 Pixel
type Pixel = Word8

-- | Creates a new image from a list of pixels.
create :: Size -> [Pixel] -> GreyImage
create size xs = fromList (imageShape size) xs

-- | Loads an image as greyscale at system path and detects image\'s type.
load :: FilePath -> IO GreyImage
load path = fromRGBA `fmap` R.load path

-- | Saves a greyscale image.
save :: FilePath -> GreyImage -> IO ()
save path = R.save path . toRGBA

-- | Gets a pixel from the image.
getPixel :: GreyImage -> Point -> Pixel
getPixel image (Point x y) = image ! (Z :. y :. x)
{-# INLINE getPixel #-}

-- | Gets a pixel from the image without checking bounds.
unsafeGetPixel :: GreyImage -> Point -> Pixel
unsafeGetPixel image (Point x y) = image `unsafeIndex` (Z :. y :. x)
{-# INLINE unsafeGetPixel #-}

-- | Gets image\'s size.
getSize :: GreyImage -> Size
getSize image =
    let (Z :. h :. w) = extent image
    in Size w h
{-# INLINE getSize #-}

-- | Resizes an image using the nearest-neighbor interpolation.
resize :: GreyImage -> Size -> GreyImage
resize image size'@(Size w' h') =
    create size' [ v |
          y' <- [0..h'-1], x' <- [0..w'-1]
        , let !x = x' * w `quot` w'
        , let !y = y' * h `quot` h'
        , let !v = image `unsafeIndex` (Z :. y :. x)
    ]
  where
    Size w h = getSize image

-- | Draws a rectangle inside the image using two transformation functions.
drawRectangle :: GreyImage
              -> (Pixel -> Pixel) -- ^ Border transformation
              -> (Pixel -> Pixel) -- ^ Background transformation
              -> Rect -> GreyImage
drawRectangle image back border (Rect x y w h) =
    image // backPts
  where
    {-borderPts = range (Point x y, Point (x+w) y) -- Top
                ++ range (Point x (y+h), Point (x+w) (y+h)) -- Bottom
                ++ range (Point x (y+1), Point x (y+h-1)) -- Left
                ++ range (Point (x+w) (y+1), Point (x+w) (y+h-1)) -- Right-}
    backPts = [ (shape, v) |
          y <- [y..y+h-1], x <- [x..x+w-1]
        , let !shape = (Z :. y :. x)
        , let !v = back $ image `unsafeIndex` shape
    ]

-- | Creates a grey image from an RGBA image.
fromRGBA :: R.RGBAImage -> GreyImage
fromRGBA image =    
    create size [ v |
          y <- [0..h-1], x <- [0..w-1]
        , let !r = (int $! image `unsafeIndex` (Z :. y :. x :. 0)) * 30
        , let !g = (int $! image `unsafeIndex` (Z :. y :. x :. 1)) * 59
        , let !b = (int $! image `unsafeIndex` (Z :. y :. x :. 2)) * 11
        , let !v =  word8 $ (r + g + b) `quot` 100
    ]
  where
    size@(Size w h) = R.getSize image
    
-- | Creates a RGBA image from an grey image.
toRGBA :: GreyImage -> R.RGBAImage
toRGBA image =
    R.create size $ concatMap (pixToRGBA . unsafeGetPixel image) coords
  where
    size@(Size w h) = getSize image
    coords = sizeRange size

    pixToRGBA pix = [pix, pix, pix, maxBound]

-- | Returns the shape of an image of the given size.
imageShape :: Size -> DIM2
imageShape (Size w h) = Z :. h :. w

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral