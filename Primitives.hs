module Primitives (
      Point (..), Size (..), Rect (..)
    ) where

import Data.Ix
import Data.Word

data Point = Point { pX :: Word16, pY :: Word16 }
    deriving (Show, Read, Eq, Ord)
    
instance Ix Point where
    range (Point x1 y1, Point x2 y2) =
        map (uncurry Point) $ range ((x1, y1), (x2, y2))
    
    index (Point x1 y1, Point x2 y2) (Point x y) = 
        index ((x1, y1), (x2, y2)) (x, y)
        
    inRange (Point x1 y1, Point x2 y2) (Point x y) =
        inRange ((x1, y1), (x2, y2)) (x, y)

    rangeSize (Point x1 y1, Point x2 y2) =
        rangeSize ((x1, y1), (x2, y2))

data Size = Size { sWidth :: Word16, sHeight :: Word16 }
    deriving (Show, Read, Eq)

data Rect = Rect {
      rX :: Word16, rY :: Word16
    , rWidth :: Word16, rHeight :: Word16
    } deriving (Show, Read, Eq)