module GreyImage (
    -- * Types & constructors
      GreyImage, Pixel (..)
    -- * Filesystem images manipulations
    , load, save
    -- * Transforms between grey and RGB images
    , fromRgb, toRgb
    -- * Functions
    , getPixel, drawRectangle
    ) where

import Control.Monad
import qualified Data.Array as A (bounds, elems)
import Data.Array.Unboxed (UArray, listArray, (!), bounds, elems, accum)
import Data.Array.ST (runSTUArray)
import Data.Array.MArray (thaw, readArray, writeArray)
import Data.Ix
import Data.Word

import Primitives
import qualified Image as RGB (Image, Pixel (..), load, save)

type GreyImage = UArray Point Pixel
type Pixel = Word8

-- | Loads an image as grey at system path and detects image\'s type.
load :: FilePath -> Maybe Size -> IO GreyImage
load path size = fromRgb `fmap` RGB.load path size

-- | Saves an grey image.
save :: FilePath -> GreyImage -> IO ()
save path = RGB.save path . toRgb

-- | Gets a pixel at (x, y).
getPixel :: GreyImage -> Point -> Pixel
getPixel image coord = image ! coord

-- | Draws a rectangle inside the image using two transformation functions.
drawRectangle :: GreyImage
              -> (Pixel -> Pixel) -- ^ Border transformation
              -> (Pixel -> Pixel) -- ^ Background transformation
              -> Rect -> GreyImage
drawRectangle image back border (Rect x y w h) =
    -- Copies into a ST Array, apply transforms and freeze
    runSTUArray $
        thaw image >>= trans backCoords back >>= trans borderCoords border
  where
    borderCoords = [] {-range (Point x y, Point (x+w) y) -- Top
                ++ range (Point x (y+h), Point (x+w) (y+h)) -- Bottom
                ++ range (Point x (y+1), Point x (y+h-1)) -- Left
                ++ range (Point (x+w) (y+1), Point (x+w) (y+h-1)) -- Right-}
    backCoords = range (Point (x+1) (y+1), Point (x+w-2) (y+h-2))
    trans cs f img = do
        forM_ cs $ \c -> do
            readArray img c >>= writeArray img c
        return img

-- | Creates a grey image from an RGB image.
fromRgb :: RGB.Image -> GreyImage
fromRgb image = listArray (A.bounds image) $ map pixToGrey $ A.elems image
  where
    pixToGrey pix =
        -- Uses eye perception of colors
        let r = (fromIntegral $ RGB.red pix) * 30
            g = (fromIntegral $ RGB.green pix) * 59
            b = (fromIntegral $ RGB.blue pix) * 11
        in fromIntegral $ (r + g + b) `quot` (100 :: Int)

-- | Creates a RGB image from an grey image.
toRgb :: GreyImage -> RGB.Image
toRgb image = listArray (bounds image) $ map pixToRGB $ elems image
  where
    pixToRGB pix = RGB.Pixel { RGB.red = pix, RGB.green = pix, RGB.blue = pix }
