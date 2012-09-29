{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Window (
    -- * Types & constructors
      Win (..), win
    -- * Constants
    , windowWidth, windowHeight, sizeIncr, moveIncr, maxPyramDeep
    -- * Functions
    , getValue, normalizeSum, windows, randomWindows, randomImagesWindows
    , nWindows
    ) where

-- import Data.Array.IArray (listArray, (!))
-- import Data.Array.Unboxed (UArray)
import Data.Int
import Data.List
import Data.Ratio
import System.Random (RandomGen, randomR, next, mkStdGen)

import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Point (..), Size (..), Rect (..))

-- | Used as a structure to iterate an image and compute features values.
data Win = Win {
      wRect :: {-# UNPACK #-} !Rect
    , wIntegral :: !II.IntegralImage
    , wAvg :: {-# UNPACK #-} !Float, wSig :: {-# UNPACK #-}  !Float

-- -- | Values ([0;255]) of the cumulative normal distribution for each
-- -- pixels values ([0; 255]) based on the average and the standard derivation
-- -- of the 'Win' pixels values.
-- -- 
-- -- Used to equalise the values inside 'Win' histogram. This will return the
-- -- value of the pixel of value @x@ in the equalised distribution:
-- -- 
-- -- > wDistibution win ! x
-- , wDistibution :: !(UArray Int Float)
    }

-- | Default window\'s size.
windowWidth, windowHeight :: Int
windowWidth = 24
windowHeight = 24

-- | Defines how the algorithm iterates the window on the image. 
-- Increments the size by sizeIncr and moves the windows by moveIncr * the 
-- current sizeIncr of the window.
sizeIncr, moveIncr :: Rational
sizeIncr = 1.25
moveIncr = 1.5

-- | Defines how large will be the smallest windows.
maxPyramDeep :: Int
maxPyramDeep = 16

-- | Constructs a new 'Win' object, computing the cumulative normal
-- distribution using the standard derivation and the average of pixels values.
win :: Rect -> II.IntegralImage -> II.IntegralImage -> Win
win rect@(Rect _ _ w h) ii sqii =
    Win rect ii avg sig
--     Win rect ii (listArray (0, 255) distribution)
  where
    avg = valuesSum / n
    sig = max 1 $ sqrt $ (squaresSum / n) - avg^(2 :: Int)
    n = float $ w * h
    valuesSum = float $ II.sumRectangle ii rect
    squaresSum = float $ II.sumRectangle sqii rect
{-# INLINE win #-}

-- -- The normal distribution of the window
-- !sqrt2Pi = 2.5066282746310002 -- Precomputes some terms
-- !a = 1 / (sig * sqrt2Pi)
-- !b = -1 / (2 * sig^(2 :: Int))
-- normal x = a * exp ((x - avg)^(2 :: Int) * b)
-- 
-- -- The accumulative function of the normal distribution
-- -- distribution = tail $ scanl (\acc x -> acc + normal x * 255) 0 [0..255]
-- distribution = go 0 0
-- go acc x | x > 255   = []
--          | otherwise = let !acc' = acc + normal x * 255
--                        in acc' : go acc' (x + 1)

-- | Gets the value of a point (in default window coordinates) inside a window.
getValue :: Win -> Point -> (Int64, Int)
Win (Rect winX winY w h) ii _ _ `getValue` Point x y =
-- Win (Rect winX winY w h) ii _ `getValue` Point x y =
    (ii `I.getPixel` Point destX destY, destX * destY)
  where
    -- New coordinates taking care of the window\'s ratio
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
normalizeSum (Win _ _ avg sig) standardN n s =
    truncate $ ((float s - (float n * avg)) / sig) * (float standardN)
{-# INLINE normalizeSum #-}
-- normalizeSum (Win _ _ distribution) standardN n s =
--     round $ float standardN * normalize (s `quot` int64 n)
--   where
--     normalize p = distribution ! int p
-- {-# INLINE normalizeSum #-}

-- | Returns, for the size of an image, the size of all possible windows with
-- their associated movement increment and their number of possible windows 
-- displacements on the horizontal and vertical axis.
windowsSizes :: Size -> [(Size, Rational, Int, Int)]
windowsSizes (Size width height) = [ (Size w h, moveIncr', nMovesX, nMovesY)
    |  sizeMult <- sizeMults
    , let w = round $ sizeMult * rational windowWidth
    , let h = round $ sizeMult * rational windowHeight
    , let moveIncr' = sizeMult * moveIncr
    , let nMovesX = floor (rational (width - w) / moveIncr') + 1
    , let nMovesY = floor (rational (height - h) / moveIncr') + 1
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
    | (Size w h, moveIncr', nMovesX, nMovesY) <- windowsSizes imageSize
    , y <- map round $ take nMovesY $ iterate (+ moveIncr') 0
    , x <- map round $ take nMovesX $ iterate (+ moveIncr') 0
    ]
  where
    imageSize = II.originalSize ii

-- | Generates an infinite list of random windows from the integral images.
randomWindows :: RandomGen g =>
                 g -> II.IntegralImage -> II.IntegralImage -> [Win]
randomWindows initGen ii sqii
    | nWins == 0 = []
    | otherwise  = go initGen
  where
    go !gen =
        -- Selects a window by choosing a random integer lesser than the number
        -- of windows, finds the corresponding size and computes the
        -- coordinates.
        let (iWindow, gen') = randomR (0, nWins - 1) gen
            Just ((rangeStart, _), (Size w h, moveIncr', nMovesX, _nMovesY)) =
                find (\((_, rangeStop), _) -> iWindow < rangeStop) sizes
            (yIndex, xIndex) = (iWindow - rangeStart) `quotRem` nMovesX
            x = round $ rational xIndex * moveIncr'
            y = round $ rational yIndex * moveIncr'
        in win (Rect x y w h) ii sqii : go gen'

    -- Contains the index ranges ([start; stop[) for each window sizes.
    sizes :: [((Int, Int), (Size, Rational, Int, Int))]
    sizes = goSize sizes' 0

    goSize []                                   _          = []
    goSize (size@(_, _, nMovesX, nMovesY) : ss) rangeStart =
        let rangeStop = rangeStart + nMovesX * nMovesY
        in ((rangeStart, rangeStop), size) : goSize ss rangeStop

    nWins = nWindows imageSize
    sizes' = windowsSizes imageSize

    imageSize = II.originalSize ii
-- {-# INLINE randomWindows #-}

-- | Given a list of integral images, returns an infinite random list of
-- windows.
-- The first window comes from the first image, the second window from
-- the second image and so on.
randomImagesWindows :: RandomGen g =>
                       g -> [(II.IntegralImage, II.IntegralImage)] -> [Win]
randomImagesWindows gen imgs =
    go imgsWindows []
    where
    -- Consumes the list of infinite lists of windows by taking a window
    -- from each list at a time.
    -- > [ [a1, a2, a3 ..], [b1, b2, b3 ..], [c1, c2, c3 ..] ]
    -- becomes:
    -- > [ a1, b1, c1, a2, b2, c2, a3, b3, c3 .. ]
    go []           acc =
        go (reverse acc) []
    go ~((x:xs):ys) acc =
        x : go ys (xs:acc)

    -- Returns the list of the infinite random lists of windows for each
    -- image.
    imgsWindows = [ randomWindows (mkStdGen (rand * i)) ii sqii
        | (i, (ii, sqii)) <- zip [1..] imgs
        ]

    rand = fst $ next gen

-- | Returns the number of different windows in an image of the given size.
nWindows :: Size -> Int
nWindows size = sum [ nMovesX * nMovesY
    | (_, _, nMovesX, nMovesY) <- windowsSizes size
    ]

float :: Integral a => a -> Float
float = fromIntegral
rational :: Integral a => a -> Rational
rational  = fromIntegral
integer :: Integral a => a -> Integer
integer = fromIntegral
-- int :: Integral a => a -> Int
-- int = fromIntegral
-- int64 :: Integral a => a -> Int64
-- int64 = fromIntegral