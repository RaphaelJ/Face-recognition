module Vision.Images.Image (
    -- * Types & constructors
      Image, Pixel (..) 
    -- * Filesystem images manipulations
    , load, save
    -- * Functions
    , getPixel, getSize
) where

import Control.Monad
import Data.Array.MArray (newArray_)
import Data.Array.Unboxed (UArray, (!), bounds)
import Data.Array.ST (newArray_, runSTUArray, writeArray)
import Data.Ix (range)
import Data.Word
import System.FilePath.Posix (takeExtension)

import qualified Codec.Image.DevIL as IL

import Vision.Primitives (Point (..), Size (..))

-- | Image (y, x, channel)
type Image = UArray (Int, Int, Int) Word8
data Pixel = Pixel { red :: Word8, green :: Word8, blue :: Word8 }

-- | Loads an image at system path and detects image\'s type.
load :: FilePath -> IO Image
load path = do
    IL.ilInit
    IL.readImage path

-- | Saves an image.
save :: FilePath -> Image -> IO ()
save path image = do
    IL.ilInit
    IL.writeImage path image

-- | Gets a pixel from the image.
getPixel :: Image -> Point -> Pixel
getPixel image (Point x y) =
    let (x', y') = (x, y)
    in Pixel {
          red = image ! (y', x', 0)
        , green = image ! (y', x', 1)
        , blue = image ! (y', x', 2)
    }
    
-- | Gets image\'s size.
getSize :: Image -> Size
getSize image = 
    let (h, w, _) = snd $ bounds $ image
    in Size (w + 1) (h + 1)

-- | Resizes an image using the nearest-neighbor interpolation.
resize :: Image -> Size -> Image
resize image (Size w' h') = runSTUArray $ do
    image' <- newArray_ ((0, 0, 0), (h'-1, w'-1, 2))

    forM_ (range ((0, 0), (h'-1, w'-1))) $ \(y', x') -> do
        let (x, y) = (round $ double x' / ratioW, round $ double y' / ratioH)
            Pixel r g b = getPixel image $ Point x y
        writeArray image' (y', x', 0) r
        writeArray image' (y', x', 1) g
        writeArray image' (y', x', 2) b

    return image'
  where
    Size w h = getSize image
    ratioW = double w' / double w
    ratioH = double h' / double h

double :: Integral a => a -> Double
double = fromIntegral