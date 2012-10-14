-- | Contains the definitions of Local Binary Patterns and Local Ternary
-- Patterns on which Local Quantized Patterns are build.
-- As descibed in
-- /Hussain, Napoleon, Jurie: Face Recognition using Local Quantized Patterns/
module Vision.LQP.Pattern (
      Horiz7, Vert7, HorizVert7, 
    ) where

import Data.Bits
import Data.List
import Data.Word

import Vision.Image (
      GreyImage, GreyPixel, inImage, unsafeGetPixel, unsafeBilinearInterpol
    )
import Vision.Primitive

data LQP = Horiz7 | Vert7 | HorizVert5 | HorizVert7 | Diag5 | HorizVertDiag5
         | HorizVertDiag7 | Disk5 | Disk7
    deriving (Show, Read, Eq)

getCode :: LQP -> GreyImage -> Point -> Word

getCode Horiz7 image p@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4, x5, x6) = (x-3, x-2, x-1, x+1, x+2, x+3)
    in genCode [
          comparePixel' (Point x1 y), comparePixel' (Point x2 y)
        , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
        , comparePixel' (Point x5 y), comparePixel' (Point x6 y)
        ]

getCode Vert7 image p@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (y1, y2, y3, y4, y5, y6) = (y-3, y-2, y-1, y+1, y+2, y+3)
    in genCode [
          comparePixel' (Point x y1), comparePixel' (Point x y2)
        , comparePixel' (Point x y3), comparePixel' (Point x y4)
        , comparePixel' (Point x y5), comparePixel' (Point x y6)
        ]

getCode HorizVert5 image pt@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4) = (x-2, x-1, x+1, x+2)
        (y1, y2, y3, y4) = (y-2, y-1, y+1, y+2)
    in genCode [
          comparePixel' (Point x1 y), comparePixel' (Point x2 y)
        , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
        , comparePixel' (Point x y1), comparePixel' (Point x y2)
        , comparePixel' (Point x y3), comparePixel' (Point x y4)
        ]

getCode HorizVert7 image pt@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4, x5, x6) = (x-3, x-2, x-1, x+1, x+2, x+3)
        (y1, y2, y3, y4, y5, y6) = (y-3, y-2, y-1, y+1, y+2, y+3)
    in genCode [
            comparePixel' (Point x1 y), comparePixel' (Point x2 y)
        , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
        , comparePixel' (Point x5 y), comparePixel' (Point x6 y)
        , comparePixel' (Point x y1), comparePixel' (Point x y2)
        , comparePixel' (Point x y3), comparePixel' (Point x y4)
        , comparePixel' (Point x y5), comparePixel' (Point x y6)
        ]

getCode Diag5 image pt@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4) = (x-2, x-1, x+1, x+2)
        (y1, y2, y3, y4) = (y-2, y-1, y+1, y+2)
    in genCode [
          comparePixel' (Point x1 y1), comparePixel' (Point x2 y2)
        , comparePixel' (Point x3 y2), comparePixel' (Point x4 y1)
        , comparePixel' (Point x1 y4), comparePixel' (Point x2 y3)
        , comparePixel' (Point x3 y3), comparePixel' (Point x4 y4)
        ]

getCode Diag7 image pt@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4, x5, x6) = (x-3, x-2, x-1, x+1, x+2, x+3)
        (y1, y2, y3, y4, y5, y6) = (y-3, y-2, y-1, y+1, y+2, y+3)
    in genCode [
          comparePixel' (Point x1 y1), comparePixel' (Point x2 y2)
        , comparePixel' (Point x3 y3), comparePixel' (Point x4 y3)
        , comparePixel' (Point x5 y2), comparePixel' (Point x6 y1)
        , comparePixel' (Point x1 y6), comparePixel' (Point x2 y5)
        , comparePixel' (Point x3 y4), comparePixel' (Point x4 y4)
        , comparePixel' (Point x5 y5), comparePixel' (Point x6 y6)
        ]

getCode HorizVertDiag5 image pt@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4) = (x-2, x-1, x+1, x+2)
        (y1, y2, y3, y4) = (y-2, y-1, y+1, y+2)
    in genCode [
          comparePixel' (Point x1 y), comparePixel' (Point x2 y)
        , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
        , comparePixel' (Point x y1), comparePixel' (Point x y2)
        , comparePixel' (Point x y3), comparePixel' (Point x y4)
        , comparePixel' (Point x1 y1), comparePixel' (Point x2 y2)
        , comparePixel' (Point x3 y2), comparePixel' (Point x4 y1)
        , comparePixel' (Point x1 y4), comparePixel' (Point x2 y3)
        , comparePixel' (Point x3 y3), comparePixel' (Point x4 y4)
        ]

getCode HorizVertDiag7 image pt@(Point x y) =
    let comparePixel' = comparePixel image (image `getPixel` p)
        (x1, x2, x3, x4, x5, x6) = (x-3, x-2, x-1, x+1, x+2, x+3)
        (y1, y2, y3, y4, y5, y6) = (y-3, y-2, y-1, y+1, y+2, y+3)
    in genCode [
          comparePixel' (Point x1 y), comparePixel' (Point x2 y)
        , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
        , comparePixel' (Point x5 y), comparePixel' (Point x6 y)
        , comparePixel' (Point x y1), comparePixel' (Point x y2)
        , comparePixel' (Point x y3), comparePixel' (Point x y4)
        , comparePixel' (Point x y5), comparePixel' (Point x y6)
        , comparePixel' (Point x1 y1), comparePixel' (Point x2 y2)
        , comparePixel' (Point x3 y3), comparePixel' (Point x4 y3)
        , comparePixel' (Point x5 y2), comparePixel' (Point x6 y1)
        , comparePixel' (Point x1 y6), comparePixel' (Point x2 y5)
        , comparePixel' (Point x3 y4), comparePixel' (Point x4 y4)
        , comparePixel' (Point x5 y5), comparePixel' (Point x6 y6)
        ]

get Disk5 image pt@(Point x y) =(image `getPixel` p)
    let center = image `getPixel` p
        comparePixel' = comparePixel image center
        compareDPixel' = compareDPixel image center
        (dX, dY) = (double x, double y)
        (x1, x2, x3, x4) = (x-2, x-1, x+1, x+2)
        (dX1, dX2, dX3, dX4, dX5, dX6) = (
              dX - 2 * cos (pi/6),  dX - 2 * cos (2 * pi/6), dX - cos (pi/4)
            , dX + 2 * cos (pi/6),  dX + 2 * cos (2 * pi/6), dX + cos (pi/4)
        )
        (y1, y2, y3, y4) = (y-2, y-1, y+1, y+2)
        (dY1, dY2, dY3, dY4, dY5, dY6) = (
              dY - 2 * sin (pi/6),  dY - 2 * sin (2 * pi/6), dY - sin (pi/4)
            , dY + 2 * sin (pi/6),  dY + 2 * sin (2 * pi/6), dY + sin (pi/4)
        )
    in genCode [
          comparePixel' (Point x1 y), comparePixel' (Point x2 y)
        , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
        , comparePixel' (Point x y1), comparePixel' (Point x y2)
        , comparePixel' (Point x y3), comparePixel' (Point x y4)
        , compareDPixel' (DPoint dX1 dY1), compareDPixel' (DPoint dX2 dY2)
        , compareDPixel' (DPoint dX4 dY2), compareDPixel' (DPoint dX5 dY1)
        , compareDPixel' (DPoint dX3 dY3), compareDPixel' (DPoint dX3 dY6)
        , compareDPixel' (DPoint dX1 dY3), compareDPixel' (DPoint dX2 dY6)
        , compareDPixel' (DPoint dX3 dY3), compareDPixel' (DPoint dX3 dY6)
        , compareDPixel' (DPoint dX3 dY3), compareDPixel' (DPoint dX3 dY6)
        ]

-- | Checks if the pixel from the image at the given point is larger than the
-- center to be compared. Returns False if the pixel is not in the image.
comparePixel :: GreyImage -> GreyPixel -> Point -> Bool
comparePixel image center pt =
    if pt `inImage` image
       then image `unsafeGetPixel` pt > center
       else False
-- {-# INLINE comparePixel #-}

   -- | Checks if the pixel from the image at the given point is larger than the
-- center to be compared. Returns False if the pixel is not in the image.
compareDPixel :: GreyImage -> GreyPixel -> DPoint -> Bool
compareDPixel image center pt@(DPoint x y) =
    if Point (truncate x) (truncate y) `inImage` image
       then image `unsafeBilinearInterpol` pt > center
       else False
-- {-# INLINE compareDPixel #-}

-- | Generates the binary code corresponding to the list of booleans.
-- The first boolean is the most signifiant bit of the code.
genCode :: Bits c => [Bool] -> Word
genCode =
    foldl' step 0
  where
    step acc b =
        let shifted = acc `shiftL` 1
        in if b then shifted + 1
                else shifted

word :: Integral a => a -> Word
word = fromIntegral
double :: Integral a => a -> Double
double = fromIntegral