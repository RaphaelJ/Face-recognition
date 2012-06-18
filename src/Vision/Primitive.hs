module Vision.Primitive (
    -- * Types & constructors
      Point (..), Size (..), Rect (..)
    -- * Utilities
    , sizeBounds, sizeRange
    ) where

import Data.Ix

data Point = Point { 
      pX :: {-# UNPACK #-} !Int, pY :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)
    
instance Ix Point where
    -- Remark: ranges are in row-major order (columns of a same row first)
    range (Point x1 y1, Point x2 y2) =
        map (uncurry $ flip Point) $ range ((y1, x1), (y2, x2))
    {-# INLINE range #-}
    
    index (Point x1 y1, Point x2 y2) (Point x y) = 
        index ((y1, x1), (y2, x2)) (y, x)
    {-# INLINE index #-}
        
    inRange (Point x1 y1, Point x2 y2) (Point x y) =
        inRange ((y1, x1), (y2, x2)) (y, x)
    {-# INLINE inRange #-}

    rangeSize (Point x1 y1, Point x2 y2) =
        rangeSize ((y1, x1), (y2, x2))
    {-# INLINE rangeSize #-}

data Size = Size { 
      sWidth :: {-# UNPACK #-} !Int
    , sHeight :: {-# UNPACK #-} !Int 
    } deriving (Show, Read, Eq)

data Rect = Rect {
      rX :: {-# UNPACK #-} !Int, rY :: {-# UNPACK #-} !Int
    , rWidth :: {-# UNPACK #-} !Int, rHeight :: {-# UNPACK #-}  !Int
    } deriving (Show, Read, Eq)
    
-- | Returns the bounds of coordinates of a rectangle of the given size.
sizeBounds :: Size -> (Point, Point)
sizeBounds (Size w h) = (Point 0 0, Point (w-1) (h-1))
{-# INLINE sizeBounds #-}
    
-- | Returns a list of coordinates within a rectangle of the given size.
sizeRange :: Size -> [Point]
sizeRange = range . sizeBounds
{-# INLINE sizeRange #-}