module Window (
    -- * Types & constructors
      Win (..), win
    -- * Constants
    , windowWidth, windowHeight
    -- * Functions
    , featuresPos, windows, getValue, normalizeSum
    ) where

import Data.Int
import Data.Word

import qualified IntegralImage as II
import Primitives

-- | Used as a structure to iterate an image.
data Win = Win {
      wRect :: Rect
    , wIntegral :: II.IntegralImage
    , wDeviation :: Int64
    , wAvg :: Int64
    } deriving (Show)

-- Default window\'s size
windowWidth, windowHeight :: Word16
windowWidth = 24
windowHeight = 24

-- | Constructs a new 'Win' object, computing the standard derivation and the
-- average pixels' value.
win :: Rect -> II.IntegralImage -> II.IntegralImage -> Win
win rect@(Rect x y w h) integral squaredIntegral =
    Win rect integral deriv avg
  where
    deriv = round $ sqrt $ double $ (squaresSum `quot` n) - avg^2
    n = int w * int h
    valuesSum = sumRectangle integral
    avg = valuesSum `quot` n
    squaresSum = sumRectangle squaredIntegral
    
    -- Computes the sum of the rectangle\'s surface using an 'IntegralImage'
    sumRectangle image =
        -- a ------- b
        -- -         -
        -- -    S    -
        -- -         -
        -- c ------- d
        let a = II.getValue image x y
            b = II.getValue image (x+w) y
            c = II.getValue image x (y+h)
            d = II.getValue image (x+w) (y+h)
        in d + a - b - c

-- | Lists all features positions and sizes inside the default window.
featuresPos :: Word16 -> Word16 -> [Rect]
featuresPos minWidth minHeight =
    rectangles minWidth minHeight windowWidth windowHeight

-- | Lists all windows for any positions and sizes inside an image.
windows :: II.IntegralImage -> II.IntegralImage -> [Win]
windows integral squaredIntegral =
    let Size w h = II.imageSize integral
    in [ win rect integral squaredIntegral |
            rect <- rectangles windowWidth windowHeight w h
       ]

-- | Gets the value of a point (as in the default window) inside the window,
-- takes care of the window\'s size ratio, so two points in two windows of
-- different sizes can be compared.
getValue :: Win -> Word16 -> Word16 -> Int64
getValue (Win (Rect winX winY w h) image _ _) x y =
    ratio $ II.getValue image destX destY
  where
    -- New coordinates with the window's ratio
    destX = winX + word16 (int x * int w `quot` int windowWidth)
    destY = winY + word16 (int y * int h `quot` int windowHeight)
    n = int w * int h
    -- Sum with the window\'s size ratio
    ratio v = v * int windowWidth * int windowHeight `quot` int w `quot` int h
    
-- | Sums 's' over 'n' pixels normalized by the window\'s standard derivation.
-- This way, two sums inside two windows of different size/standard derivation
-- can be compared.
normalizeSum :: Win -> Int64 -> Int64 -> Int64
normalizeSum (Win _ _ deriv avg) n s =
    n * (normalize $ s `quot` n)
  where
    -- Pixel's value normalized with the double of the standard derivation
    -- (95% of pixels values, following the normal distribution), averaged
    -- around 127.
    normalize p = (p - (avg - 2*deriv)) * 255 `quot` (4*deriv)

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

double :: (Integral a) => a -> Double
double = fromIntegral
int :: (Integral a) => a -> Int64
int = fromIntegral
word16 :: (Integral a) => a -> Word16
word16 = fromIntegral