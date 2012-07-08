{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Window (
    -- * Types & constructors
      Win (..), win
    -- * Constants
    , windowWidth, windowHeight
    -- * Functions
    , getValue, normalizeSum, windows
    ) where

import Debug.Trace
    
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Int
import Data.Word
import Data.Ratio

import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Point (..), Size (..), Rect (..))

-- | Used as a structure to iterate an image.
data Win = Win {
      wRect :: {-# UNPACK #-} !Rect
    , wIntegral :: {-# UNPACK #-} !II.IntegralImage 
    -- | Values ([0;255]) of the commulative normal distribution for each 
    -- pixels values ([0; 255]) based on the average and the standard derivation
    -- of the 'Win' pixels values.
    -- 
    -- Used to equalize the values inside 'Win' histogram. This will return the
    -- value of the pixel of value @x@ in the equalized distribution:
    -- 
    -- > wDistibution win ! x
    , wDistibution :: {-# UNPACK #-} !(UArray Int Double)
    }

-- Default window\'s size.
windowWidth, windowHeight :: Int
windowWidth = 24
windowHeight = 24

-- | Constructs a new 'Win' object, computing the commulative normal
-- distribution using the standard derivation and the average of pixels values.
win :: Rect -> II.IntegralImage -> II.IntegralImage -> Win
win rect@(Rect x y w h) integral squaredIntegral =
    Win rect integral (listArray (0, 255) distribution)
  where
    avg = valuesSum / n
    sig = max 1 $ sqrt $ (squaresSum / n) - avg^2
    n = double $ w * h
    valuesSum = double $ II.sumRectangle integral rect
    squaresSum = double $ II.sumRectangle squaredIntegral rect
    
    -- The normal distribution of the window
    !a = 1 / (sig * sqrt (2 * pi)) -- Precompute some terms
    !b = 2 * sig^2
    normal x = a * exp (-(x - avg)^2 / b)
    
    -- The accumulative function of the normal distribution
    distribution =
        tail $ scanl (\acc x -> acc + normal x * 255) 0 [0..255]
{-# INLINE win #-}

-- | Gets the value of a point (in default window coordinates) inside a window.
getValue :: Win -> Point -> (Int64, Int)
Win (Rect winX winY w h) integral _ `getValue` Point x y =
    (integral `I.getPixel` Point destX destY, destX * destY)
  where
    -- New coordinates with the window\'s ratio
    !destX = winX + ((x * w) `quot` windowWidth)
    !destY = winY + ((y * h) `quot` windowHeight)
{-# INLINE getValue #-}
    
-- | Normalizes the sum of a feature rectangle using the average and standard 
-- derivation of the window and the number of pixels of the rectangle ('n') 
-- with the number of pixels of the same rectangle in the standard window 
-- ('standardN').
-- This way, two sums inside two windows of different size/standard derivation
-- can be compared.
normalizeSum :: Win -> Int -> Int -> Int64 -> Int64
normalizeSum (Win _ _ distribution) standardN n s =
    round $ double standardN * normalize (s `quot` int64 n)
  where
    normalize p = distribution ! int p
{-# INLINE normalizeSum #-}

-- | Lists all windows for any positions and sizes inside an image.
windows :: II.IntegralImage -> II.IntegralImage -> [Win]
windows integral squaredIntegral = [ win (Rect x y w h) integral squaredIntegral
    | sizeMult <- sizeMults
    , let w = round $ sizeMult * fromIntegral windowWidth
    , let h = round $ sizeMult * fromIntegral windowHeight
    , let moveIcr' = round $ sizeMult * moveIcr
    , x <- [0,moveIcr'..width-w]
    , y <- [0,moveIcr'..height-h]
    ]
  where
    sizeIncr = 1.25 :: Rational
    moveIcr = 1.5 :: Rational
    maxPyramDeep = 15
    
    Size iiWidth iiHeight = I.getSize integral
    (width, height) = (iiWidth - 1, iiHeight - 1)
    (width', height') = (integer width, integer height)
    maxSizeMult =
        min (width' % integer windowWidth) (height' % integer windowHeight)
    minSizeMult = max 1 (maxSizeMult / (1.25^(maxPyramDeep-1)))
    sizeMults = takeWhile (<= maxSizeMult) $ iterate (* sizeIncr) minSizeMult
{-# INLINE windows #-}

double :: Integral a => a -> Double
double = fromIntegral
integer :: Integral a => a -> Integer
integer = fromIntegral
int :: Integral a => a -> Int
int = fromIntegral
int64 :: Integral a => a -> Int64
int64 = fromIntegral
word16 :: Integral a => a -> Word16
word16 = fromIntegral