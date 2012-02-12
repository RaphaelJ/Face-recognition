module Vision.Primitives (
    -- * Types & constructors
      Point (..), Size (..), Rect (..)
    ) where

import Data.Ix
import Data.Word

data Point = Point { pX :: Int, pY :: Int }
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

data Size = Size { sWidth :: Int, sHeight :: Int }
    deriving (Show, Read, Eq)

data Rect = Rect {
      rX :: Int, rY :: Int
    , rWidth :: Int, rHeight :: Int
    } deriving (Show, Read, Eq)