{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Vision.Image (
    -- * Types classes
      Image (..), StorableImage (..), Convertible (..)
    -- * Misc images tranformations
    , resize, drawRectangle
    ) where

import Data.Convertible (Convertible (..), convert)

import Vision.Primitive (Point (..), Size (..), Rect (..), sizeRange)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as IL

-- | The 'Image' class represents images with pixels of type p.
-- 'Image's are 0 indexed.
-- 
-- Minimal complete definition: 'fromList', 'getSize' and 'getPixel'.
class Image i p | i -> p where
    fromList :: Size -> [p] -> i
    
    fromFunction :: Size -> (Point -> p) -> i
    
    getSize :: i -> Size
    
    getPixel, unsafeGetPixel :: i -> Point -> p
        
    fromFunction size f = fromList size [ f p | p <- sizeRange size ]
    {-# INLINE fromFunction #-}
    
    unsafeGetPixel = getPixel
    {-# INLINE unsafeGetPixel #-} 

-- | The 'StorableImage' class adds storage capabilities to images.
class StorableImage i where
    load :: FilePath -> IO i
    save :: FilePath -> i -> IO ()

instance (Convertible IL.Image i, Convertible i IL.Image)
         => StorableImage i where
    load path =
        IL.runIL $ convert `fmap` IL.readImage path
    {-# INLINE load #-}
        
    save path i = 
        IL.runIL $ IL.writeImage path (convert i)
    {-# INLINE save #-}

-- | Resizes the 'Image' using the nearest-neighbor interpolation.
resize :: Image i p => i -> Size -> i
resize image size'@(Size w' h') =
    fromFunction size' $ \(Point x' y') ->
        let x = x' * w `quot` w'
            y = y' * h `quot` h'
        in image `unsafeGetPixel` Point x y
  where
    Size w h = getSize image
{-# INLINABLE resize #-}

-- | Draws a rectangle inside the 'Image' using two transformation functions.
drawRectangle :: Image i p => i
              -> (p -> p) -- ^ Border transformation
              -> (p -> p) -- ^ Background transformation
              -> Rect -> i
drawRectangle image back border (Rect rx ry rw rh) =
    fromFunction (getSize image) $ \pt -> draw pt (image `unsafeGetPixel` pt)
  where
    draw pt pix
        | inBackground pt = back pix
        | inBorder pt     = border pix
        | otherwise       = pix
      
    inBackground (Point x y) =
        x > rx && y > ry && x < rx' && y < ry'
    
    inBorder (Point x y) =
        x >= rx && y >= ry && x <= rx' && y <= ry'

    rx' = rx + rw - 1
    ry' = ry + rh - 1
{-# INLINABLE drawRectangle #-}