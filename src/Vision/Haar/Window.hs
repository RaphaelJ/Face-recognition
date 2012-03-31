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

import Data.Int
import Data.Word

import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Point (..), Size (..), Rect (..))

-- | Used as a structure to iterate an image.
data Win = Win {
      wRect :: !Rect
    , wIntegral :: !II.IntegralImage
    , wDeviation :: !Int64 -- ^ Int64 to avoid casting
    , wAvg :: !Int64
    } deriving (Show)

-- Default window\'s size
windowWidth, windowHeight :: Int
windowWidth = 24
windowHeight = 24

-- | Constructs a new 'Win' object, computing the standard derivation and the
-- average pixels' value.
win :: Rect -> II.IntegralImage -> II.IntegralImage -> Win
win rect@(Rect x y w h) integral squaredIntegral =
    Win rect integral deriv avg
  where
    deriv = max 1 $ round $ sqrt $ double $ (squaresSum `quot` n) - avg^2
    n = int64 w * int64 h
    valuesSum = sumRectangle integral
    avg = valuesSum `quot` n
    squaresSum = sumRectangle squaredIntegral
    
    -- Computes the sum of the windows\'s surface using an 'IntegralImage'
    sumRectangle image =
        -- a ------- b
        -- -         -
        -- -    S    -
        -- -         -
        -- c ------- d
        let a = image `I.getPixel` Point x y
            b = image `I.getPixel` Point (x+w) y
            c = image `I.getPixel` Point x (y+h)
            d = image `I.getPixel` Point (x+w) (y+h)
        in d + a - b - c

-- | Gets the value of a point (as in the default window) inside the window,
-- takes care of the window\'s size ratio, so two points in two windows of
-- different sizes can be compared.
getValue :: Win -> Point -> Int64
getValue (Win (Rect winX winY w h) image _ _) (Point x y) =
    {-ratio $! -} image `I.getPixel` Point destX destY
  where
    -- New coordinates with the window's ratio
    !destX = winX + (x * w `quot` windowWidth)
    !destY = winY + (y * h `quot` windowHeight)
    !n = int64 w * int64 h
    -- Sum with the window\'s size ratio
    ratio v = v * int64 windowWidth * int64 windowHeight `quot` int64 w `quot` int64 h
{-# INLINE getValue #-}
    
-- | Sums 's' over 'n' pixels normalized by the window\'s standard derivation.
-- This way, two sums inside two windows of different size/standard derivation
-- can be compared.
normalizeSum :: Win -> Int64 -> Int64 -> Int64
normalizeSum (Win _ _ deriv avg) n s =
    n * (normalize $! s `quot` n)
  where
    -- Pixel\'s value normalized with the double of the standard derivation
    -- (95% of pixels values, following the normal distribution), averaged
    -- around 127.
    normalize p = (p - (avg - 2*deriv)) * 255 `quot` (4*deriv)

-- | Lists all features positions and sizes inside the default window.
featuresPos :: Int -> Int -> [Rect]
featuresPos minWidth minHeight =
    rectangles minWidth minHeight windowWidth windowHeight

-- | Lists all windows for any positions and sizes inside an image.
windows :: II.IntegralImage -> II.IntegralImage -> [Win]
windows integral squaredIntegral =
    let Size w h = I.getSize integral
    in [ win rect integral squaredIntegral |
            rect <- rectangles windowWidth windowHeight w h
       ]

-- | Lists all rectangle positions and sizes inside a rectangle of
-- width * height.
rectangles minWidth minHeight width height =
    [ Rect x y w h |
          x <- [0,incrX..width-minWidth]
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
int64 :: Integral a => a -> Int64
int64 = fromIntegral
word16 :: Integral a => a -> Word16
word16 = fromIntegral