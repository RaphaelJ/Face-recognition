module Vision.Image.RGBAImage (
    -- * Types & constructors
      RGBAImage, Pixel (..), create
    -- * Filesystem images manipulations
    , load, save
    -- * Functions
    , getPixel, getSize, resize, imageBounds, imageRange
) where

import Control.Monad
import Data.Array.MArray (newArray_)
import Data.Array.Unboxed (UArray, listArray, (!), bounds)
import Data.Array.ST (newArray_, runSTUArray, writeArray)
import Data.Ix (range)
import Data.Word

import qualified Codec.Image.DevIL as IL

import Vision.Primitive (Point (..), Size (..), sizeRange)

-- | RGBA image (y, x, channel)
type RGBAImage = UArray (Int, Int, Int) Word8
data Pixel = Pixel {
    red :: Word8, green :: Word8, blue :: Word8, alpha :: Word8
    } deriving (Show, Read)

-- | Creates a new image from a list of pixels.
create :: Size -> [Word8] -> RGBAImage
create = listArray . imageBounds

-- | Loads an image at system path and detects image\'s type.
load :: FilePath -> IO RGBAImage
load path = do
    IL.ilInit
    IL.readImage path

-- | Saves an image.
save :: FilePath -> RGBAImage -> IO ()
save path image = do
    IL.ilInit
    IL.writeImage path image

-- | Gets a pixel from the image.
getPixel :: RGBAImage -> Point -> Pixel
getPixel image (Point x y) =
    Pixel {
          red = image ! (y, x, 0)
        , green = image ! (y, x, 1)
        , blue = image ! (y, x, 2)
        , alpha = image ! (y, x, 3)
    }
    
-- | Gets image\'s size.
getSize :: RGBAImage -> Size
getSize image = 
    let (h, w, _) = snd $ bounds $ image
    in Size (w + 1) (h + 1)

-- | Resizes an image using the nearest-neighbor interpolation.
resize :: RGBAImage -> Size -> RGBAImage
resize image size'@(Size w' h') = runSTUArray $ do
    image' <- newArray_ $ imageBounds size'

    forM_ (sizeRange size') $ \(Point y' x') -> do
        let (x, y) = (x' * w `quot` w', y' * h `quot` h')
            Pixel r g b a = getPixel image $ Point x y
        writeArray image' (y', x', 0) r
        writeArray image' (y', x', 1) g
        writeArray image' (y', x', 2) b
        writeArray image' (y', x', 2) a

    return image'
  where
    Size w h = getSize image

-- | Returns the bounds of coordinates of the image.
imageBounds :: Size -> ((Int, Int, Int), (Int, Int, Int))
imageBounds (Size w h) = ((0, 0, 0), (h-1, w-1, 3))
    
-- | Returns the list of the coordinates of the image.
imageRange :: Size -> [(Int, Int, Int)]
imageRange = range . imageBounds

int :: Integral a => a -> Int
int = fromIntegral
double :: Integral a => a -> Double
double = fromIntegral