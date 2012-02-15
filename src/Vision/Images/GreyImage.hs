module Vision.Images.GreyImage (
    -- * Types & constructors
      GreyImage, Pixel (..), create
    -- * Filesystem images manipulations
    , load, save
    -- * Functions
    , getPixel, getSize, resize, drawRectangle
    -- * Transforms between grey and RGB images
    , fromRgb, toRgb
    ) where

import Control.Monad
import Data.Array.MArray (thaw, readArray, writeArray)
import Data.Array.ST (newArray_, runSTUArray)
import Data.Array.Unboxed (UArray, listArray, (!), bounds, elems)
import Data.Ix
import Data.Word

import qualified Vision.Images.Image as RGB
import Vision.Primitives (
    Point (..), Size (..), Rect (..), sizeBounds, sizeRange
    )

-- | Greyscale image (y, x).
type GreyImage = UArray Point Pixel
type Pixel = Word8

-- | Creates a new image from a list of pixels.
create :: Size -> [Pixel] -> GreyImage
create = listArray . sizeBounds

-- | Loads an image as grey at system path and detects image\'s type.
load :: FilePath -> IO GreyImage
load path = fromRgb `fmap` RGB.load path

-- | Saves a grey image.
save :: FilePath -> GreyImage -> IO ()
save path = RGB.save path . toRgb

-- | Gets a pixel from the image.
getPixel :: GreyImage -> Point -> Pixel
getPixel image coord = image ! coord

-- | Gets image\'s size.
getSize :: GreyImage -> Size
getSize image =
    let Point w h = snd $ bounds $ image
    in Size (w + 1) (h + 1)

-- | Resizes an image using the nearest-neighbor interpolation.
resize :: GreyImage -> Size -> GreyImage
resize image size@(Size w' h') = runSTUArray $ do
    image' <- newArray_ bounds'

    forM_ (range bounds') $ \coords'@(Point x' y') -> do
        let (x, y) = (round $ double x' / ratioW, round $ double y' / ratioH)
        writeArray image' coords' $ getPixel image (Point x y)
    
    return image'
  where
    bounds' = sizeBounds size
    Size w h = getSize image
    ratioW = double w' / double w
    ratioH = double h' / double h

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
fromRgb image = 
    create size $ map (pixToGrey . RGB.getPixel image) coords
  where
    size = RGB.getSize image
    coords = sizeRange size

    pixToGrey pix =
        -- Uses human eye perception of colors
        let r = (int $ RGB.red pix) * 30
            g = (int $ RGB.green pix) * 59
            b = (int $ RGB.blue pix) * 11
        in word8 $ (r + g + b) `quot` 100
    
-- | Creates a RGB image from an grey image.
toRgb :: GreyImage -> RGB.Image
toRgb image =
    RGB.create size $ concatMap (pixToRGB . getPixel image) coords
  where
    size = getSize image
    coords = sizeRange size
    
    pixToRGB pix = [pix, pix, pix, maxBound]

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
double :: Integral a => a -> Double
double = fromIntegral