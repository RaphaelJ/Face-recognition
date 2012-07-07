{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Window (
    -- * Types & constructors
      Win (..), win
    -- * Constants
    , windowWidth, windowHeight
    -- * Functions
    , getValue, normalizeSum, featuresPos, windows
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

windowPixels :: Int64
windowPixels = 
    let !v = int64 $ windowWidth * windowWidth
    in v

-- | Constructs a new 'Win' object, computing the commulative normal
-- distribution using the standard derivation and the average of pixels values.
win :: Rect -> II.IntegralImage -> II.IntegralImage -> Win
win rect@(Rect x y w h) integral squaredIntegral =
    Win rect integral distribution
  where
    avg = valuesSum / n
    sig = max 1 $ sqrt $ (squaresSum / n) - avg^2
    n = double $ w * h
    valuesSum = double $ II.sumRectangle integral rect
    squaresSum = double $ II.sumRectangle squaredIntegral rect
    sqrt2Pi = sqrt (2 * pi)
    -- The normal distribution of the window
    normal x = (1 / (sig * sqrt2Pi)) * exp (-(x - avg)^2 / (2 * sig^2))
    -- The accumulative function of the normal distribution
    distribution =
        listArray (0, 255) $ tail $ scanl (\acc x -> acc + normal x * 255) 0 [0..255]

-- | Gets the value of a point (as in the default window) inside the window,
-- takes care of the window\'s size ratio, so two points in two windows of
-- different sizes can be compared.
getValue :: Win -> Point -> Int64
Win (Rect winX winY w h) integral _ `getValue` Point x y =
    integral `I.getPixel` Point destX destY
  where
--     v = ratio $ integral `I.getPixel` Point destX destY
    -- New coordinates with the window\'s ratio
    destX = winX + ((x * w) `quot` windowWidth)
    destY = winY + ((y * h) `quot` windowHeight)
--     standardX = ((winX * windowWidth) `quot` w) + x
--     standardY = ((winY * windowHeight) `quot` h) + y
--     n = int64 $ destX * destY
--     standardN = int64 $ standardX * standardY
--     -- Sum with the window\'s size ratio
--     ratio v = 
--         if n == 0 then 0 -- Division by zero
--                   else v * standardN `quot` n
--         v * standardN `quot` (n + 1)
{-# INLINABLE getValue #-}
    
-- | Sums 's' over 'n' pixels normalized by the window\'s standard derivation.
-- This way, two sums inside two windows of different size/standard derivation
-- can be compared.
normalizeSum :: Win -> Int -> Int64 -> Int64
normalizeSum (Win (Rect _ _ w h) _ distribution) n s =
    round $ double n * (normalize $ s * int64 windowPixels `quot` int64 w `quot` int64  h `quot` int64 n)
  where
    normalize p = 
        if p > 255 then {-trace (" " ++ show p)-} (distribution ! 255)
                   else if p < 0 then {-trace (" " ++ show p)-} (distribution ! 0)
                                 else {-traceShow p $ -}distribution ! int p
{-# INLINABLE normalizeSum #-}

-- | Lists all features positions and sizes inside the default window.
featuresPos :: Int -> Int -> [Rect]
featuresPos minWidth minHeight =
    rectangles minWidth minHeight windowWidth windowHeight

-- | Lists all windows for any positions and sizes inside an image.
windows :: II.IntegralImage -> II.IntegralImage -> [Win]
windows integral squaredIntegral =
    [ win (Rect x y w h) integral squaredIntegral
    | size <- traceShow (head sizes) sizes
    , let w = round $ size * fromIntegral windowWidth
    , let h = round $ size * fromIntegral windowHeight
    , let incrX' = round $ size * incrX
    , let incrY' = round $ size * incrY
    , x <- [0,incrX'..width-w]
    , y <- [0,incrY'..height-h]
    ]
  where
    sizeIncr = 125 % 100
    incrX = 2
    incrY = 2
    maxPyramDeep = 15
    Size iWidth iHeight = I.getSize integral
    (width, height) = (iWidth - 1, iHeight - 1)
    maxSize = min (width `quot` windowWidth) (height `quot` windowHeight)
    minWidth = (width % maxPyramDeep) / fromIntegral windowWidth
    minHeight = (height % maxPyramDeep) / fromIntegral windowHeight
    minSize = max 1 (max minWidth minHeight)
    sizes = takeWhile (<= fromIntegral maxSize) $ iterate (* sizeIncr) minSize

-- | Lists all rectangles positions and sizes inside a rectangle of
-- width * height.
rectangles minWidth minHeight width height =
    [ Rect x y w h 
    | x <- [0,incrX..width-minWidth]
    , y <- [0,incrY..height-minHeight]
    , w <- [minWidth,minWidth+incrWidth..width-x]
    , h <- [minHeight,minHeight+incrHeight..height-y]
    ]
  where
    incrMult = 5
    incrX = 1 * incrMult
    incrY = 1 * incrMult
    incrWidth = minWidth * incrMult
    incrHeight = minHeight * incrMult

double :: Integral a => a -> Double
double = fromIntegral
int :: Integral a => a -> Int
int = fromIntegral
int64 :: Integral a => a -> Int64
int64 = fromIntegral
word16 :: Integral a => a -> Word16
word16 = fromIntegral