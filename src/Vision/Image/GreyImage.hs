module Vision.Image.GreyImage (
    -- * Types & constructors
      GreyImage, Pixel (..), create
    -- * Filesystem images manipulations
    , load, save
    -- * Functions
    , getPixel, getSize, resize, drawRectangle, imageBounds, imageRange
    -- * Transforms between greyscale and RGBA images
    , fromRgba, toRgba
    ) where

import Control.Monad
import Data.Array.MArray (thaw, readArray, writeArray)
import Data.Array.ST (newArray_, runSTUArray)
import Data.Array.Unboxed (UArray, listArray, (!), bounds, accum)
import Data.Ix
import Data.Word

import qualified Vision.Image.RGBAImage as R
import Vision.Primitive (
    Point (..), Size (..), Rect (..), sizeRange
    )

-- | Greyscale image (y, x).
type GreyImage = UArray (Int, Int) Pixel
type Pixel = Word8

-- | Creates a new image from a list of pixels.
create :: Size -> [Pixel] -> GreyImage
create = listArray . imageBounds

-- | Loads an image as greyscale at system path and detects image\'s type.
load :: FilePath -> IO GreyImage
load path = fromRgba `fmap` R.load path

-- | Saves a greyscale image.
save :: FilePath -> GreyImage -> IO ()
save path = R.save path . toRgba

-- | Gets a pixel from the image.
getPixel :: GreyImage -> Point -> Pixel
getPixel image (Point x y) = image ! (y, x)

-- | Gets image\'s size.
getSize :: GreyImage -> Size
getSize image =
    let (h, w) = snd $ bounds $ image
    in Size (w + 1) (h + 1)

-- | Resizes an image using the nearest-neighbor interpolation.
resize :: GreyImage -> Size -> GreyImage
resize image size@(Size w' h') = runSTUArray $ do
    image' <- newArray_ bounds'

    forM_ (range bounds') $ \coords'@(x', y') -> do
        let (x, y) = (round $ double x' / ratioW, round $ double y' / ratioH)
        writeArray image' coords' $ getPixel image (Point x y)
    
    return image'
  where
    bounds' = imageBounds size
    Size w h = getSize image
    ratioW = double w' / double w
    ratioH = double h' / double h

-- | Draws a rectangle inside the image using two transformation functions.
drawRectangle :: GreyImage
              -> (Pixel -> Pixel) -- ^ Border transformation
              -> (Pixel -> Pixel) -- ^ Background transformation
              -> Rect -> GreyImage
drawRectangle image back border (Rect x y w h) =
    accum (\p f -> f p) image (backPts ++ borderPts)
--     -- Copies into a ST Array, apply transforms and freeze
--     
--     runSTUArray $
--         thaw image >>= trans backCoords back >>= trans borderCoords border
  where
    borderPts = [] {-range (Point x y, Point (x+w) y) -- Top
                ++ range (Point x (y+h), Point (x+w) (y+h)) -- Bottom
                ++ range (Point x (y+1), Point x (y+h-1)) -- Left
                ++ range (Point (x+w) (y+1), Point (x+w) (y+h-1)) -- Right-}
    backPts = applyFct back $ range ((x+1, y+1), (x+w-2, y+h-2))
    -- Puts f in a tuple with each value of the list
    applyFct f xs = zip xs (repeat f)
--     trans cs f img = do
--         forM_ cs $ \c -> do
--             readArray img c >>= writeArray img c
--         return img

-- | Creates a grey image from an RGBA image.
fromRgba :: R.RGBAImage -> GreyImage
fromRgba image = 
    create size $ map (pixToGrey . R.getPixel image) coords
  where
    size = R.getSize image
    coords = sizeRange size

    pixToGrey pix =
        -- Uses human eye perception of colors
        let r = (int $ R.red pix) * 30
            g = (int $ R.green pix) * 59
            b = (int $ R.blue pix) * 11
        in word8 $ (r + g + b) `quot` 100
    
-- | Creates a RGB image from an grey image.
toRgba :: GreyImage -> R.RGBAImage
toRgba image =
    R.create size $ concatMap (pixToRGBA . getPixel image) coords
  where
    size = getSize image
    coords = sizeRange size
    
    pixToRGBA pix = [pix, pix, pix, maxBound]

-- | Returns the bounds of coordinates of the image.
imageBounds :: Size -> ((Int, Int), (Int, Int))
imageBounds (Size w h) = ((0, 0), (h-1, w-1))

-- | Returns the list of the coordinates of the image.
imageRange :: Size -> [(Int, Int)]
imageRange = range . imageBounds

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
double :: Integral a => a -> Double
double = fromIntegral