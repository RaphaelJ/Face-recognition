{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Vision.Image.IImage (
    -- * Types classes
      Image (..), Pixel (..), StorableImage (..), Convertible (..)
    -- * Functions
    , convert, bilinearInterpol
    -- * Misc images transformations
    , resize, drawRectangle, horizontalFlip
    ) where

import Data.Convertible (Convertible (..), convert)

import Vision.Primitive (
      Point (..), DPoint (..), Size (..), Rect (..), sizeRange
    )

import qualified Data.Array.Repa.IO.DevIL as IL

-- | The 'Image' class represents images with pixels of type 'p' which contains
-- values of type 'a'.
-- 'Image's are 0 indexed.
-- 
-- Minimal complete definition: 'fromList', 'getSize' and 'getPixel'.
class Pixel p a => Image i p a | i -> p a where
    fromList :: Size -> [p] -> i
    toList :: i -> [p]

    fromFunction :: Size -> (Point -> p) -> i

    getSize :: i -> Size

    getPixel, unsafeGetPixel :: i -> Point -> p

    -- | Force the evaluation of the underlying array (default definition is
    -- 'undefined'). Useful for images with a delayed representation.
    force :: i -> i

    toList i = map (i `unsafeGetPixel`) (sizeRange $ getSize i)
    {-# INLINE toList #-}

    fromFunction size f = fromList size [ f p | p <- sizeRange size ]
    {-# INLINE fromFunction #-}

    unsafeGetPixel = getPixel
    {-# INLINE unsafeGetPixel #-}

    force = undefined

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
    save :: i -> FilePath -> IO ()

-- | Makes every image showable.
instance (Image i p a, Show p) => Show i where
    show = show . toList

-- | Makes every image comparable.
instance (Image i p a, Eq p) => Eq i where
    a == b = toList a == toList b

-- | Makes every image convertible from and to a DevIL's Image storable into
-- a file.
instance (Convertible IL.Image i, Convertible i IL.Image)
         => StorableImage i where
    load path =
        IL.runIL $ convert `fmap` IL.readImage path
    {-# INLINE load #-}

    save i path =
        IL.runIL $ IL.writeImage path (convert i)
    {-# INLINE save #-}

-- | Uses a bilinear interpolation to find the value of the pixel at the
-- floating point coordinates.
-- Estimates the value of P using A, B, C and D :
-- q ------ r
-- -        -
-- -  P     -
-- -        -
-- s ------ t
bilinearInterpol, unsafeBilinearInterpol 
    :: (Image i p a, Integral a) => i -> DPoint -> p
i `bilinearInterpol` p@(DPoint x y) = 
    if x >= 0 && y >= 0 && x < fromIntegral w && y < fromIntegral h
       then i `unsafeBilinearInterpol` p
       else error "Invalid index"
  where
    Size w h = getSize i

-- | Uses a bilinear interpolation without checking bounds.
i `unsafeBilinearInterpol` DPoint x y =
    valuesToPix $ interpolateChannels qs rs ss ts
  where
    (x1, y1) = (truncate x, truncate y)
    (x2, y2) = (x1 + 1, y1 + 1)
    (d_x1, d_y1, d_x2, d_y2) =
        (double x1, double y1, double x2, double y2)

    -- Interpolate each channel of the four pixels.
    interpolateChannels []        _       _        _        = []
    interpolateChannels ~(q:qs') ~(r:rs') ~(s:ss') ~(t:ts') =
        interpolate q r s t : interpolateChannels qs' rs' ss' ts'
    interpolate q r s t =
        let (q', r') = (double q, double r)
            (s', t') = (double s, double t)
        in round $
              q' * (d_x2 - x) * (d_y2 - y) + r' * (x - d_x1) * (d_y2 - y)
            + s' * (d_x2 - x) * (y - d_y1) + t' * (x - d_x1) * (y - d_y1)

    qs = pixToValues $ i `unsafeGetPixel` Point x1 y1
    rs = pixToValues $ i `unsafeGetPixel` Point x2 y1
    ss = pixToValues $ i `unsafeGetPixel` Point x1 y2
    ts = pixToValues $ i `unsafeGetPixel` Point x2 y2
{-# INLINE bilinearInterpol #-}
{-# INLINE unsafeBilinearInterpol #-}

-- | Resizes the 'Image' using a bilinear interpolation.
resize :: (Image i p a, Integral a) => i -> Size -> i
resize image size'@(Size w' h') =
    fromFunction size' $ \(Point x' y') ->
        let x = double x' * widthRatio
            y = double y' * heightRatio
        in image `unsafeBilinearInterpol` DPoint x y
  where
    Size w h = getSize image
    widthRatio = double w / double w'
    heightRatio = double h / double h'
{-# INLINABLE resize #-} -- With INLINABLE, the function can be specialised.

-- | Draws a rectangle inside the 'IImage' using two transformation functions.
drawRectangle :: Image i p a => i
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
{-# INLINE drawRectangle #-}

-- | Reverses the image horizontally.
horizontalFlip :: Image i p a => i -> i
horizontalFlip image =
    fromFunction (getSize image) $ \(Point x' y) ->
        let x = maxX - x'
        in image `unsafeGetPixel` Point x y
  where
    Size w _ = getSize image
    maxX = w - 1
{-# INLINABLE horizontalFlip #-}

double :: Integral a => a -> Double
double = fromIntegral