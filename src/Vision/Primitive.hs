module Vision.Primitive (
    -- * Types & constructors
      Point (..), Size (..), Rect (..)
    -- * Utilities
    , sizeBounds, sizeRange
    ) where

import Data.Ix

data Point = Point { pX :: !Int, pY :: !Int }
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

data Size = Size { sWidth :: !Int, sHeight :: !Int }
    deriving (Show, Read, Eq)

data Rect = Rect {
      rX :: !Int, rY :: !Int
    , rWidth :: !Int, rHeight :: !Int
    } deriving (Show, Read, Eq)
    
-- | Returns the bounds of coordinates of a rectangle of the given size.
sizeBounds :: Size -> (Point, Point)
sizeBounds (Size w h) = (Point 0 0, Point (w-1) (h-1))
{-# INLINE sizeBounds #-}
    
-- | Returns a list of coordinates within a rectangle of the given size.
sizeRange :: Size -> [Point]
sizeRange = range . sizeBounds
{-# INLINE sizeRange #-}