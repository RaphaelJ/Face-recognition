{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Window (
    -- * Types & constructors
      Win (..), win
    -- * Constants
    , windowWidth, windowHeight, sizeIncr, moveIcr, maxPyramDeep
    -- * Functions
    , getValue, normalizeSum, windows, randomWindows, nWindows
    ) where

import Data.Array (Array)
import Data.Array.IArray (bounds, listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Ratio
import System.Random (RandomGen, randomR)

import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Point (..), Size (..), Rect (..))

-- | Used as a structure to iterate an image.
data Win = Win {
      wRect :: {-# UNPACK #-} !Rect
    , wIntegral :: {-# UNPACK #-} !II.IntegralImage 
    -- | Values ([0;255]) of the cumulative normal distribution for each 
    -- pixels values ([0; 255]) based on the average and the standard derivation
    -- of the 'Win' pixels values.
    -- 
    -- Used to equalise the values inside 'Win' histogram. This will return the
    -- value of the pixel of value @x@ in the equalised distribution:
    -- 
    -- > wDistibution win ! x
    , wDistibution :: {-# UNPACK #-} !(UArray Int Double)
    }

-- | Default window\'s size.
windowWidth, windowHeight :: Int
windowWidth = 24
windowHeight = 24

-- | Defines how the algorithm iterate the window on the image. 
sizeIncr, moveIcr :: Rational
sizeIncr = 1.25
moveIcr = 1.5

-- | Defines how large will be the smallest windows.
maxPyramDeep :: Int
maxPyramDeep = 16

-- | Constructs a new 'Win' object, computing the cumulative normal
-- distribution using the standard derivation and the average of pixels values.
win :: Rect -> II.IntegralImage -> II.IntegralImage -> Win
win rect@(Rect _ _ w h) ii sqii =
    Win rect ii (listArray (0, 255) distribution)
  where
    avg = valuesSum / n
    sig = max 1 $ sqrt $ (squaresSum / n) - avg^(2 :: Int)
    n = double $ w * h
    valuesSum = double $ II.sumRectangle ii rect
    squaresSum = double $ II.sumRectangle sqii rect
    
    -- The normal distribution of the window
    !a = 1 / (sig * sqrt (2 * pi)) -- Precompute some terms
    !b = 2 * sig^(2 :: Int)
    normal x = a * exp (-(x - avg)^(2 :: Int) / b)
    
    -- The accumulative function of the normal distribution
    distribution =
        tail $ scanl (\acc x -> acc + normal x * 255) 0 [0..255]
{-# INLINE win #-}

-- | Gets the value of a point (in default window coordinates) inside a window.
getValue :: Win -> Point -> (Int64, Int)
Win (Rect winX winY w h) ii _ `getValue` Point x y =
    (ii `I.getPixel` Point destX destY, destX * destY)
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

-- | Returns, for the size of an image, the size of all possible windows with
-- their associated movement increment and their number of possible windows 
-- displacements on the horizontal and vertical axis.
windowsSizes :: Size -> [(Size, Rational, Int, Int)]
windowsSizes (Size width height) = [ (Size w h, moveIcr', nMovesX, nMovesY)
    |  sizeMult <- sizeMults
    , let w = round $ sizeMult * rational windowWidth
    , let h = round $ sizeMult * rational windowHeight
    , let moveIcr' = sizeMult * moveIcr
    , let nMovesX = floor (rational (width - w) / moveIcr') + 1
    , let nMovesY = floor (rational (height - h) / moveIcr') + 1
    ]
  where
    (width', height') = (integer width, integer height)
    -- Create a pyramid of windows with a maximum size which always equals the 
    -- smallest side of the image.
    maxSizeMult =
        min (width' % integer windowWidth) (height' % integer windowHeight)
    minSizeMult = max 1 (maxSizeMult / (sizeIncr^(maxPyramDeep-1)))
    sizeMults = takeWhile (<= maxSizeMult) $ iterate (* sizeIncr) minSizeMult

-- | Lists all windows for any positions and sizes inside an image using two
-- 'IntegralImage's (normal and squared).
windows :: II.IntegralImage -> II.IntegralImage -> [Win]
windows ii sqii = [ win (Rect x y w h) ii sqii
    | (Size w h, moveIcr', nMovesX, nMovesY) <- windowsSizes imageSize
    , x <- map round $ take nMovesX $ iterate (+ moveIcr') 0
    , y <- map round $ take nMovesY $ iterate (+ moveIcr') 0
    ]
  where
    imageSize = II.originalSize ii
{-# INLINE windows #-}

-- | Generates an infinite list of random windows from the integral images.
randomWindows :: RandomGen g => 
                 g -> II.IntegralImage -> II.IntegralImage -> [Win]
randomWindows initGen ii sqii =
    go initGen
  where
    go !gen = 
        let (sizeIndex, gen') = randomR (bounds sizes) gen
            (Size w h, moveIcr', nMovesX, nMovesY) = sizes ! sizeIndex
            (xIndex, gen'') = randomR (0, nMovesX - 1) gen'
            x = round (rational xIndex * moveIcr')
            (yIndex, gen''') = randomR (0, nMovesY - 1) gen''
            y = round (rational yIndex * moveIcr')
        in win (Rect x y w h) ii sqii : go gen'''
    
    sizes :: Array Int (Size, Rational, Int, Int)
    sizes = listArray (0, length sizes' - 1) sizes'
    
    sizes' = windowsSizes imageSize
    imageSize = II.originalSize ii
-- {-# INLINE randomWindows #-}

-- | Returns the number of different windows in an image of the given size.
nWindows :: Size -> Int
nWindows size = sum [ nMovesX * nMovesY
    | (_, _, nMovesX, nMovesY) <- windowsSizes size
    ]

double :: Integral a => a -> Double
double = fromIntegral
rational :: Integral a => a -> Rational
rational  = fromIntegral
integer :: Integral a => a -> Integer
integer = fromIntegral
int :: Integral a => a -> Int
int = fromIntegral
int64 :: Integral a => a -> Int64
int64 = fromIntegral