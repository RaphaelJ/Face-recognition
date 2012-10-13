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

import Vision.Image (GreyImage, GreyPixel, unsafeGetPixel, inImage)

-- | Class for patterns 'p' which computes a binary code 'c'.
class Pattern p c | p -> c where
    -- | Computes the binary code from a point of the image.
    getCode :: p -> GreyImage -> Point -> c

    -- | Gives the size in bits of the binary code.
    codeSize :: p -> Int

data Horiz7

instance Pattern Horiz7 Word8 where
    getCode _ i p@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
            (x1, x2, x3, x4, x5, x6) = (x-3, x-2, x-1, x+1, x+2, x+3)
        in genCode [
              comparePixel' (Point x1 y), comparePixel' (Point x2 y)
            , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
            , comparePixel' (Point x5 y), comparePixel' (Point x6 y)
            ]

    codeSize _ = 6

data Vert7

instance Pattern Vert7 Word8 where
    getCode _ i p@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
            (y1, y2, y3, y4, y5, y6) = (y-3, y-2, y-1, y+1, y+2, y+3)
        in genCode [
              comparePixel' (Point x y1), comparePixel' (Point x y2)
            , comparePixel' (Point x y3), comparePixel' (Point x y4)
            , comparePixel' (Point x y5), comparePixel' (Point x y6)
            ]

    codeSize _ = 6

data HorizVert5

instance Pattern HorizVert5 Word8 where
    getCode _ image pt@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
            (x1, x2, x3, x4) = (x-2, x-1, x+1, x+2)
            (y1, y2, y3, y4) = (y-2, y-1, y+1, y+2)
        in genCode [
              comparePixel' (Point x1 y), comparePixel' (Point x2 y)
            , comparePixel' (Point x3 y), comparePixel' (Point x4 y)
            , comparePixel' (Point x y1), comparePixel' (Point x y2)
            , comparePixel' (Point x y3), comparePixel' (Point x y4)
            ]

    codeSize _ = 8

data HorizVert7

instance Pattern HorizVert7 Word16 where
    getCode _ image pt@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
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

    codeSize _ = 12

data Diag5

instance Pattern Diag5 Word8 where
    getCode _ image pt@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
            (x1, x2, x3, x4) = (x-2, x-1, x+1, x+2)
            (y1, y2, y3, y4) = (y-2, y-1, y+1, y+2)
        in genCode [
              comparePixel' (Point x1 y1), comparePixel' (Point x2 y2)
            , comparePixel' (Point x3 y2), comparePixel' (Point x4 y1)
            , comparePixel' (Point x1 y4), comparePixel' (Point x2 y3)
            , comparePixel' (Point x3 y3), comparePixel' (Point x4 y4)
            ]

    codeSize _ = 8

data Diag7

instance Pattern Diag7 Word16 where
    getCode _ image pt@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
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

    codeSize _ = 12

data HorizVertDiag5

instance Pattern HorizVertDiag5 Word16 where
    getCode _ image pt@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
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

    codeSize _ = 16

data HorizVertDiag7

instance Pattern HorizVertDiag7 Word32 where
    getCode _ image pt@(Point x y) =
        let comparePixel' = comparePixel image (i `getPixel` p)
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

    codeSize _ = 24

-- | Checks if the pixel from the image at the given point is larger than the
-- center to be compared. Returns False if the pixel is not in the image.
comparePixel :: GreyImage -> GreyPixel -> Point -> Bool
comparePixel image center pt =
    if pt `inImage` image
       then image `unsafeGetPixel` pt > center
       else False
-- {-# INLINE comparePixel #-}

-- | Generates the binary code corresponding to the list of booleans.
-- The first boolean is the most signifiant bit of the code.
genCode :: Bits c => [Bool] -> c
genCode =
    foldl' step 0
  where
    step acc b =
        let shifted = acc `shiftL` 1
        in if b then shifted + 1
                else shifted
-- {-# SPECIALIZE genCode :: [Bool] -> Word8 #-}
-- {-# SPECIALIZE genCode :: [Bool] -> Word16 #-}
-- {-# SPECIALIZE genCode :: [Bool] -> Word32 #-}

word :: Integral a => a -> Word
word = fromIntegral