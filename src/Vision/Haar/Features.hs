module Vision.Haar.Features (
    -- * Types & constructors
      HaarFeature (..)
    -- * Functions
    , compute, features
    ) where

import Data.Int
    
import qualified Vision.Haar.Window as W
import Vision.Images.IntegralImage (IntegralImage, computeIntegralImage)
import Vision.Primitives

data HaarFeature = TwoVertRect Rect
                 | TwoHorizRect Rect
                 | ThreeVertRect Rect
                 | ThreeHorizRect Rect
                 | FourRect Rect
    deriving (Show, Read, Eq)

compute :: HaarFeature -> W.Win -> Int64
compute (TwoVertRect (Rect x y w h)) win =
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
        a = W.getValue win x y
        b = W.getValue win (x+w) y
        c = W.getValue win x (y+h')
        d = W.getValue win (x+w) (y+h')
        e = W.getValue win x (y+h)
        f = W.getValue win (x+w) (y+h)
        normalize = W.normalizeSum win (int w * int h)
        s1 = normalize $ d + a - b - c
        s2 = normalize $ f + c - d - e
    in s2 - s1
    
compute (TwoHorizRect (Rect x y w h)) win =
    -- a ------- b ------- c
    -- -         -         -
    -- -   S1    -   S2    -
    -- -         -         -
    -- d ------- e ------- f
    let w' = w `quot` 2
        a = W.getValue win x y
        b = W.getValue win (x+w') y
        c = W.getValue win (x+w) y
        d = W.getValue win x (y+h)
        e = W.getValue win (x+w') (y+h)
        f = W.getValue win (x+w) (y+h)
        normalize = W.normalizeSum win (int w * int h)
        s1 = normalize $ e + a - b - d
        s2 = normalize $ f + b - c - e
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
    -- g ------- h''
    let h' = h `quot` 3
        a = W.getValue win x y
        b = W.getValue win (x+w) y
        c = W.getValue win x (y+h')
        d = W.getValue win (x+w) (y+h')
        e = W.getValue win x (y+h'+h')
        f = W.getValue win (x+w) (y+h'+h')
        g = W.getValue win x (y+h)
        h'' = W.getValue win (x+w) (y+h)
        normalize = W.normalizeSum win (int w * int h)
        s1 = normalize $ d + a - b - c
        s2 = normalize $ f + c - d - e
        s3 = normalize $ h'' + e - f - g
    in s1 + s3 - s2
        
compute (ThreeHorizRect (Rect x y w h)) win =
    -- a ------- b ------- c ------- d
    -- -         -         -         -
    -- -   S1    -   S2    -   S3    -
    -- -         -         -         -
    -- e ------- f ------- g ------- h''
    let w' = w `quot` 3
        a = W.getValue win x y
        b = W.getValue win (x+w') y
        c = W.getValue win (x+w'+w') y
        d = W.getValue win (x+w) y
        e = W.getValue win x (y+h)
        f = W.getValue win (x+w') (y+h)
        g = W.getValue win (x+w'+w') (y+h)
        h'' = W.getValue win (x+w) (y+h)
        normalize = W.normalizeSum win (int w * int h)
        s1 = normalize $ f + a - b - e
        s2 = normalize $ g + b - c - f
        s3 = normalize $ h'' + c - d - g
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
    -- g ------ h'' ------ i
    let w' = w `quot` 2
        h' = h `quot` 2
        a = W.getValue win x y
        b = W.getValue win (x+w') y
        c = W.getValue win (x+w) y
        d = W.getValue win x (y+h')
        e = W.getValue win (x+w') (y+h')
        f = W.getValue win (x+w) (y+h')
        g = W.getValue win x (y+h)
        h'' = W.getValue win (x+w') (y+h)
        i = W.getValue win (x+w) (y+h)
        normalize = W.normalizeSum win (int w * int h)
        s1 = normalize $ e + a - b - d
        s2 = normalize $ f + b - c - d
        s3 = normalize $ h'' + d - e - g
        s4 = normalize $ i + e - f - h''
    in s1 + s4 - s2 - s3

-- | List all features inside a standard window.
features :: [HaarFeature]
features =
    map TwoVertRect (W.featuresPos 1 2) ++
    map TwoHorizRect (W.featuresPos 2 1) ++
    map ThreeVertRect (W.featuresPos 1 3) ++
    map ThreeHorizRect (W.featuresPos 3 1) ++
    map FourRect (W.featuresPos 2 2)

int :: (Integral a) => a -> Int64
int = fromIntegral