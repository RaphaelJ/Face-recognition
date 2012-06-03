{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Vision.Image.IImage (
    -- * Types classes
      Image (..), Pixel (..), StorableImage (..), Convertible (..)
    ) where

import Data.Convertible (Convertible (..), convert)

import Vision.Primitive (Point (..), Size (..), Rect (..), sizeRange)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as IL

-- | The 'Image' class represents images with pixels of type 'p' which contains
-- values of type 'a'.
-- 'Image's are 0 indexed.
-- 
-- Minimal complete definition: 'fromList', 'getSize' and 'getPixel'.
class Pixel p a => Image i p a | i -> p a where
    fromList :: Size -> [p] -> i
    
    fromFunction :: Size -> (Point Int -> p) -> i
    
    getSize :: i -> Size
    
    getPixel, unsafeGetPixel :: i -> Point Int -> p
        
    fromFunction size f = fromList size [ f p | p <- sizeRange size ]
    {-# INLINE fromFunction #-}
    
    unsafeGetPixel = getPixel
    {-# INLINE unsafeGetPixel #-} 

-- | Permits the application of a function to each value 'a' of a pixel 'p'.
-- Used to apply a transformation to each channel of an image.
--
-- Minimal complete definition: 'pixToValues' and 'valuesToPix'.
class Pixel p a | p -> a where
    pixToValues :: p -> [a]
    valuesToPix :: [a] -> p
    
    pixApply :: p -> (a -> a) -> p
    pixApply pix f = valuesToPix $ map f $ pixToValues pix
    {-# INLINE pixApply #-}

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