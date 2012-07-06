{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Feature (
    -- * Types & constructors
      HaarFeature (..)
    -- * Functions
    , compute, features
    ) where

import Debug.Trace
    
import Data.Int
    
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
        a = win `W.getValue` Point x y
        b = win `W.getValue` Point (x+w) y
        c = win `W.getValue` Point x (y+h')
        d = win `W.getValue` Point (x+w) (y+h')
        e = win `W.getValue` Point x (y+h)
        f = win `W.getValue` Point (x+w) (y+h)
        n = w * h `quot` 2
        !s1 = W.normalizeSum win n $ d + a - b - c
        !s2 = W.normalizeSum win n $ f + c - d - e
    in s2 - s1
    
compute (TwoHorizRect (Rect x y w h)) win =
    -- a ------- b ------- c
    -- -         -         -
    -- -   S1    -   S2    -
    -- -         -         -
    -- d ------- e ------- f
    let w' = w `quot` 2
        a = win `W.getValue` Point x y
        b = win `W.getValue` Point (x+w') y
        c = win `W.getValue` Point (x+w) y
        d = win `W.getValue` Point x (y+h)
        e = win `W.getValue` Point (x+w') (y+h)
        f = win `W.getValue` Point (x+w) (y+h)
        n = w * h `quot` 2
        !s1 = W.normalizeSum win n $ e + a - b - d
        !s2 = W.normalizeSum win n $ f + b - c - e
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
        a = win `W.getValue` Point x y
        b = win `W.getValue` Point (x+w) y
        c = win `W.getValue` Point x (y+h')
        d = win `W.getValue` Point (x+w) (y+h')
        e = win `W.getValue` Point x (y+h'+h')
        f = win `W.getValue` Point (x+w) (y+h'+h')
        g = win `W.getValue` Point x (y+h)
        i = win `W.getValue` Point (x+w) (y+h)
        n = w * h * 2 `quot` 3
        !s1 = W.normalizeSum win n $ d + a - b - c
        !s2 = W.normalizeSum win n $ f + c - d - e
        !s3 = W.normalizeSum win n $ i + e - f - g
    in s1 + s3 - s2
        
compute (ThreeHorizRect (Rect x y w h)) win =
    -- a ------- b ------- c ------- d
    -- -         -         -         -
    -- -   S1    -   S2    -   S3    -
    -- -         -         -         -
    -- e ------- f ------- g ------- h''
    let w' = w `quot` 3
        a = win `W.getValue` Point x y
        b = win `W.getValue` Point (x+w') y
        c = win `W.getValue` Point (x+w'+w') y
        d = win `W.getValue` Point (x+w) y
        e = win `W.getValue` Point x (y+h)
        f = win `W.getValue` Point (x+w') (y+h)
        g = win `W.getValue` Point (x+w'+w') (y+h)
        i = win `W.getValue` Point (x+w) (y+h)
        n = w * h * 2 `quot` 3
        !s1 = W.normalizeSum win n $ f + a - b - e
        !s2 = W.normalizeSum win n $ g + b - c - f
        !s3 = W.normalizeSum win n $ i + c - d - g
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
        a = win `W.getValue` Point x y
        b = win `W.getValue` Point (x+w') y
        c = win `W.getValue` Point (x+w) y
        d = win `W.getValue` Point x (y+h')
        e = win `W.getValue` Point (x+w') (y+h')
        f = win `W.getValue` Point (x+w) (y+h')
        g = win `W.getValue` Point x (y+h)
        i = win `W.getValue` Point (x+w') (y+h)
        j = win `W.getValue` Point (x+w) (y+h)
        n = w * h * 2 `quot` 4
        !s1 = W.normalizeSum win n $ e + a - b - d
        !s2 = W.normalizeSum win n $ f + b - c - e
        !s3 = W.normalizeSum win n $ i + d - e - g
        !s4 = W.normalizeSum win n $ j + e - f - i
    in s1 + s4 - s2 - s3
{-# INLINE compute #-}

-- | List all features inside a standard window.
features :: [HaarFeature]
features =
    map TwoVertRect (W.featuresPos 1 2) ++
    map TwoHorizRect (W.featuresPos 2 1) ++
    map ThreeVertRect (W.featuresPos 1 3) ++
    map ThreeHorizRect (W.featuresPos 3 1) ++
    map FourRect (W.featuresPos 2 2)

int64 :: (Integral a) => a -> Int64
int64 = fromIntegral