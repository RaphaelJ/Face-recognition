{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Feature (
    -- * Types & constructors
      HaarFeature (..)
    -- * Constants
    , sizeMult, moveMult
    -- * Functions
    , compute, features, featuresPos
    ) where

import Data.Int

import qualified Vision.Haar.Window as W
import Vision.Primitive (Point (..), Rect (..))

data HaarFeature = TwoVertRect {-# UNPACK #-} !Rect
                 | TwoHorizRect {-# UNPACK #-} !Rect
                 | ThreeVertRect {-# UNPACK #-} !Rect
                 | ThreeHorizRect {-# UNPACK #-} !Rect
                 | FourRect {-# UNPACK #-} !Rect
    deriving (Show, Read, Eq)

sizeMult, moveMult :: Int
sizeMult = 2
moveMult = 2

compute :: HaarFeature -> W.Win -> Int64
compute (TwoVertRect (Rect x y w h)) win =
    --     x         x'
    -- 
    -- y   a ------- b
    --     -         -
    --     -   S1    -
    --     -         -
    -- y'  c ------- d
    --     -         -
    --     -   S2    -
    --     -         -
    -- y'' e ------- f
    let h' = h `quot` 2
        (x', y', y'') = (x+w, y+h', y+h)
        (a, aN) = win `W.getValue` Point x  y
        (b, bN) = win `W.getValue` Point x' y
        (c, cN) = win `W.getValue` Point x  y'
        (d, dN) = win `W.getValue` Point x' y'
        (e, eN) = win `W.getValue` Point x  y''
        (f, fN) = win `W.getValue` Point x' y''
        n = w * h'
        !s1 = W.normalizeSum win n (dN + aN - bN - cN) (d + a - b - c)
        !s2 = W.normalizeSum win n (fN + cN - dN - eN) (f + c - d - e)
    in s2 - s1

compute (TwoHorizRect (Rect x y w h)) win =
    --    x         x'        x''
    -- 
    -- y  a ------- b ------- c
    --    -         -         -
    --    -   S1    -   S2    -
    --    -         -         -
    -- y' d ------- e ------- f
    let w' = w `quot` 2
        (x', x'', y') = (x+w', x+w, y+h)
        (a, aN) = win `W.getValue` Point x   y
        (b, bN) = win `W.getValue` Point x'  y
        (c, cN) = win `W.getValue` Point x'' y
        (d, dN) = win `W.getValue` Point x   y'
        (e, eN) = win `W.getValue` Point x'  y'
        (f, fN) = win `W.getValue` Point x'' y'
        n = w' * h
        !s1 = W.normalizeSum win n (eN + aN - bN - dN) (e + a - b - d)
        !s2 = W.normalizeSum win n (fN + bN - cN - eN) (f + b - c - e)
    in s2 - s1

compute (ThreeVertRect (Rect x y w h)) win =
    --      x         x'
    -- 
    -- y    a ------- b
    --      -         -
    --      -   S1    -
    --      -         -
    -- y'   c ------- d
    --      -         -
    --      -   S2    -
    --      -         -
    -- y''  e ------- f
    --      -         -
    --      -   S3    -
    --      -         -
    -- y''' g ------- i
    let h' = h `quot` 3
        (x', y', y'', y''') = (x+w, y+h', y+h'+h', y+h)
        (a, aN) = win `W.getValue` Point x  y
        (b, bN) = win `W.getValue` Point x' y
        (c, cN) = win `W.getValue` Point x  y'
        (d, dN) = win `W.getValue` Point x' y'
        (e, eN) = win `W.getValue` Point x  y''
        (f, fN) = win `W.getValue` Point x' y''
        (g, gN) = win `W.getValue` Point x  y'''
        (i, iN) = win `W.getValue` Point x' y'''
        n = w * h'
        !s1 = W.normalizeSum win n (dN + aN - bN - cN) (d + a - b - c)
        !s2 = W.normalizeSum win n (fN + cN - dN - eN) (f + c - d - e)
        !s3 = W.normalizeSum win n (iN + eN - fN - gN) (i + e - f - g)
    in s1 + s3 - s2

compute (ThreeHorizRect (Rect x y w h)) win =
    --    x         x'        x''       x'''
    -- 
    -- y  a ------- b ------- c ------- d
    --    -         -         -         -
    --    -   S1    -   S2    -   S3    -
    --    -         -         -         -
    -- y' e ------- f ------- g ------- i
    let w' = w `quot` 3
        (x', x'', x''', y') = (x+w', x+w'+w', x+w, y+h)
        (a, aN) = win `W.getValue` Point x    y
        (b, bN) = win `W.getValue` Point x'   y
        (c, cN) = win `W.getValue` Point x''  y
        (d, dN) = win `W.getValue` Point x''' y
        (e, eN) = win `W.getValue` Point x    y'
        (f, fN) = win `W.getValue` Point x'   y'
        (g, gN) = win `W.getValue` Point x''  y'
        (i, iN) = win `W.getValue` Point x''' y'
        n = w' * h
        !s1 = W.normalizeSum win n (fN + aN - bN - eN) (f + a - b - e)
        !s2 = W.normalizeSum win n (gN + bN - cN - fN) (g + b - c - f)
        !s3 = W.normalizeSum win n (iN + cN - dN - gN) (i + c - d - g)
    in s1 + s3 - s2

compute (FourRect (Rect x y w h)) win =
    --     x         x'        x''
    -- 
    -- y   a ------- b ------- c
    --     -         -         -
    --     -   S1    -    S2   -
    --     -         -         -
    -- y'  d ------- e ------- f
    --     -         -         -
    --     -   S3    -    S4   -
    --     -         -         -
    -- y'' g ------- i ------- j
    let w' = w `quot` 2
        h' = h `quot` 2
        (x', x'', y', y'') = (x+w', x+w, y+h', y+h)
        (a, aN) = win `W.getValue` Point x   y
        (b, bN) = win `W.getValue` Point x'  y
        (c, cN) = win `W.getValue` Point x'' y
        (d, dN) = win `W.getValue` Point x   y'
        (e, eN) = win `W.getValue` Point x'  y'
        (f, fN) = win `W.getValue` Point x'' y'
        (g, gN) = win `W.getValue` Point x   y''
        (i, iN) = win `W.getValue` Point x'  y''
        (j, jN) = win `W.getValue` Point x'' y''
        n = w' * h'
        !s1 = W.normalizeSum win n (eN + aN - bN - dN) (e + a - b - d)
        !s2 = W.normalizeSum win n (fN + bN - cN - eN) (f + b - c - e)
        !s3 = W.normalizeSum win n (iN + dN - eN - gN) (i + d - e - g)
        !s4 = W.normalizeSum win n (jN + eN - fN - iN) (j + e - f - i)
    in s1 + s4 - s2 - s3

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

-- | Lists all rectangles positions and sizes inside a rectangle of
-- width * height.
rectangles :: Int -> Int -> Int -> Int -> [Rect]
rectangles minWidth minHeight width height = [ Rect x y w h 
    | x <- [0,incrX..width-minWidth]
    , y <- [0,incrY..height-minHeight]
    , w <- [minWidth,minWidth+incrWidth..width-x]
    , h <- [minHeight,minHeight+incrHeight..height-y]
    ]
  where
    incrX = 1 * moveMult
    incrY = 1 * moveMult
    incrWidth = minWidth * sizeMult
    incrHeight = minHeight * sizeMult