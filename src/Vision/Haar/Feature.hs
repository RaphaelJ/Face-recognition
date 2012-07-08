{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Feature (
    -- * Types & constructors
      HaarFeature (..)
    -- * Functions
    , compute, features, featuresPos
    ) where

import Debug.Trace
    
import Data.Int
import Data.Ratio

import Data.List
    
import qualified Vision.Haar.Window as W
import Vision.Primitive (Point (..), Rect (..))

data HaarFeature = TwoVertRect !Rect
                 | TwoHorizRect !Rect
                 | ThreeVertRect !Rect
                 | ThreeHorizRect !Rect
                 | FourRect !Rect
    deriving (Show, Read, Eq)

compute :: HaarFeature -> W.Win -> Int64
compute r@(TwoVertRect (Rect x y w h)) win =
    -- a ------- b
    -- -         -
    -- -   S1    -
    -- -         -
    -- c ------- d
    -- -         -
    -- -   S2    -
    -- -         -
    -- e ------- f
    let h' = h `quot` 2
        (a, aN) = win `W.getValue` Point x y
        (b, bN) = win `W.getValue` Point (x+w) y
        (c, cN) = win `W.getValue` Point x (y+h')
        (d, dN) = win `W.getValue` Point (x+w) (y+h')
        (e, eN) = win `W.getValue` Point x (y+h)
        (f, fN) = win `W.getValue` Point (x+w) (y+h)
        n = w * h'
        !s1 = W.normalizeSum win n (dN + aN - bN - cN) (d + a - b - c)
        !s2 = W.normalizeSum win n (fN + cN - dN - eN) (f + c - d - e)
    in s2 - s1
    
compute (TwoHorizRect (Rect x y w h)) win =
    -- a ------- b ------- c
    -- -         -         -
    -- -   S1    -   S2    -
    -- -         -         -
    -- d ------- e ------- f
    let w' = w `quot` 2
        (a, aN) = win `W.getValue` Point x y
        (b, bN) = win `W.getValue` Point (x+w') y
        (c, cN) = win `W.getValue` Point (x+w) y
        (d, dN) = win `W.getValue` Point x (y+h)
        (e, eN) = win `W.getValue` Point (x+w') (y+h)
        (f, fN) = win `W.getValue` Point (x+w) (y+h)
        n = w' * h
        !s1 = W.normalizeSum win n (eN + aN - bN - dN) (e + a - b - d)
        !s2 = W.normalizeSum win n (fN + bN - cN - eN) (f + b - c - e)
    in s2 - s1

compute (ThreeVertRect (Rect x y w h)) win =
    -- a ------- b
    -- -         -
    -- -   S1    -
    -- -         -
    -- c ------- d
    -- -         -
    -- -   S2    -
    -- -         -
    -- e ------- f
    -- -         -
    -- -   S3    -
    -- -         -
    -- g ------- i
    let h' = h `quot` 3
        (a, aN) = win `W.getValue` Point x y
        (b, bN) = win `W.getValue` Point (x+w) y
        (c, cN) = win `W.getValue` Point x (y+h')
        (d, dN) = win `W.getValue` Point (x+w) (y+h')
        (e, eN) = win `W.getValue` Point x (y+h'+h')
        (f, fN) = win `W.getValue` Point (x+w) (y+h'+h')
        (g, gN) = win `W.getValue` Point x (y+h)
        (i, iN) = win `W.getValue` Point (x+w) (y+h)
        n = w * h'
        !s1 = W.normalizeSum win n (dN + aN - bN - cN) (d + a - b - c)
        !s2 = W.normalizeSum win n (fN + cN - dN - eN) (f + c - d - e)
        !s3 = W.normalizeSum win n (iN + eN - fN - gN) (i + e - f - g)
    in s1 + s3 - s2
        
compute (ThreeHorizRect (Rect x y w h)) win =
    -- a ------- b ------- c ------- d
    -- -         -         -         -
    -- -   S1    -   S2    -   S3    -
    -- -         -         -         -
    -- e ------- f ------- g ------- i
    let w' = w `quot` 3
        (a, aN) = win `W.getValue` Point x y
        (b, bN) = win `W.getValue` Point (x+w') y
        (c, cN) = win `W.getValue` Point (x+w'+w') y
        (d, dN) = win `W.getValue` Point (x+w) y
        (e, eN) = win `W.getValue` Point x (y+h)
        (f, fN) = win `W.getValue` Point (x+w') (y+h)
        (g, gN) = win `W.getValue` Point (x+w'+w') (y+h)
        (i, iN) = win `W.getValue` Point (x+w) (y+h)
        n = w' * h
        !s1 = W.normalizeSum win n (fN + aN - bN - eN) (f + a - b - e)
        !s2 = W.normalizeSum win n (gN + bN - cN - fN) (g + b - c - f)
        !s3 = W.normalizeSum win n (iN + cN - dN - gN) (i + c - d - g)
    in s1 + s3 - s2
    
compute (FourRect (Rect x y w h)) win =
    -- a ------- b ------- c
    -- -         -         -
    -- -   S1    -    S2   -
    -- -         -         -
    -- d ------- e ------- f
    -- -         -         -
    -- -   S3    -    S4   -
    -- -         -         -
    -- g ------- i ------- j
    let w' = w `quot` 2
        h' = h `quot` 2
        (a, aN) = win `W.getValue` Point x y
        (b, bN) = win `W.getValue` Point (x+w') y
        (c, cN) = win `W.getValue` Point (x+w) y
        (d, dN) = win `W.getValue` Point x (y+h')
        (e, eN) = win `W.getValue` Point (x+w') (y+h')
        (f, fN) = win `W.getValue` Point (x+w) (y+h')
        (g, gN) = win `W.getValue` Point x (y+h)
        (i, iN) = win `W.getValue` Point (x+w') (y+h)
        (j, jN) = win `W.getValue` Point (x+w) (y+h)
        n = w' * h'
        !s1 = W.normalizeSum win n (eN + aN - bN - dN) (e + a - b - d)
        !s2 = W.normalizeSum win n (fN + bN - cN - eN) (f + b - c - e)
        !s3 = W.normalizeSum win n (iN + dN - eN - gN) (i + d - e - g)
        !s4 = W.normalizeSum win n (jN + eN - fN - iN) (j + e - f - i)
    in s1 + s4 - s2 - s3
{-# INLINE compute #-}

-- | List all features inside a standard window.
features :: [HaarFeature]
features =
    map TwoVertRect (featuresPos 1 2) ++
    map TwoHorizRect (featuresPos 2 1) ++
    map ThreeVertRect (featuresPos 1 3) ++
    map ThreeHorizRect (featuresPos 3 1) ++
    map FourRect (featuresPos 2 2)
    
-- | Lists all features positions and sizes inside the default window.
featuresPos :: Int -> Int -> [Rect]
featuresPos minWidth minHeight =
    rectangles minWidth minHeight W.windowWidth W.windowHeight
{-# INLINE featuresPos #-}

-- | Lists all rectangles positions and sizes inside a rectangle of
-- width * height.
-- rectangles minWidth minHeight width height = [ Rect x y w h
--     | w <- [minWidth,minWidth*2..width], h <- [minHeight,minHeight*2..height]
--     , let incrX = round $ moveIncr * (integer w % integer minWidth)
--     , let incrY = round $ moveIncr * (integer h % integer minHeight)
--     , x <- [0,incrX..width-w], y <- [0,incrY..height-h]
--     ]
--   where
rectangles minWidth minHeight width height = [ Rect x y w h 
    | x <- [0,incrX..width-minWidth]
    , y <- [0,incrY..height-minHeight]
    , w <- [minWidth,minWidth+incrWidth..width-x]
    , h <- [minHeight,minHeight+incrHeight..height-y]
    ]
  where
    sizeMult = 3
    moveMult = 3
    
    incrX = 1 * moveMult
    incrY = 1 * moveMult
    incrWidth = minWidth * sizeMult
    incrHeight = minHeight * sizeMult
    
    moveIncr = 1 :: Rational
    
--     sizeIncrs =
--         map round $ scanl (\acc m -> acc + 1 * m) 1 $ iterate (* sizeIncr) 1
--     widths = takeWhile (<= width) $ map (* minWidth) sizeIncrs
--     heights = takeWhile (<= height) $ map (* minHeight) sizeIncrs

integer :: (Integral a) => a -> Integer
integer = fromIntegral
int64 :: (Integral a) => a -> Int64
int64 = fromIntegral