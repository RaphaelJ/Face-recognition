module GreyImage (
      GreyImage, Pixel (..)
    , load, save, getPixel, drawRectangle, fromRgb, toRgb
) where

import qualified Data.Array as A (listArray, bounds, elems)
import Data.Array.Unboxed (UArray, listArray, (!), bounds, elems, accum)
import Data.Ix
import Data.Word

import Primitives
import qualified Image as RGB (Image, Pixel (..), load, save)

type GreyImage = UArray Point Pixel
type Pixel = Word8

-- | Load an image as grey at system path and detect image's type
load :: FilePath -> Maybe Size -> IO GreyImage
load path size = fmap fromRgb $ RGB.load path size

-- | Save an grey image
save :: FilePath -> GreyImage -> IO ()
save path = RGB.save path . toRgb

-- | Get a pixel at (x, y)
getPixel :: GreyImage -> Point -> Pixel
getPixel image coord = image ! coord

-- | Draw a rectangle inside the image using two modification functions
drawRectangle :: GreyImage
              -> (Pixel -> Pixel) -- ^ Border transformation
              -> (Pixel -> Pixel) -- ^ Background transformation
              -> Rect -> GreyImage
drawRectangle image border back (Rect x y w h) =
    let borderCoords = [] {-range (Point x y, Point (x+w) y) -- Top
                    ++ range (Point x (y+h), Point (x+w) (y+h)) -- Bottom
                    ++ range (Point x (y+1), Point x (y+h-1)) -- Left
                    ++ range (Point (x+w) (y+1), Point (x+w) (y+h-1)) -- Right-}
        backCoords = range (Point (x+1) (y+1), Point (x+w-1) (y+h-1))
        tranform f cs img = accum (fAccum f) img $ map (\c -> (c, ())) cs
        fAccum f pix _ = f pix
    in tranform border borderCoords $ tranform back backCoords image

-- | Create an grey image from an RGB image
fromRgb :: RGB.Image -> GreyImage
fromRgb image = listArray (A.bounds image) $ map pixToGrey $ A.elems image
  where
    pixToGrey pix =
        -- Use eye perception of colors
        let r = (fromIntegral $ RGB.red pix) * 30
            g = (fromIntegral $ RGB.green pix) * 59
            b = (fromIntegral $ RGB.blue pix) * 11
        in fromIntegral $ (r + g + b) `div` 100

-- | Create a RGB image from an grey image
toRgb :: GreyImage -> RGB.Image
toRgb image = listArray (bounds image) $ map pixToRGB $ elems image
  where
    pixToRGB pix = RGB.Pixel { RGB.red = pix, RGB.green = pix, RGB.blue = pix }