{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.Vision.Haar (
      tests
    ) where
        
import qualified Data.Set as S
import System.Random (mkStdGen)

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import qualified Vision.Haar.Window as W

import Tests.Vision.Image ()

import Debug.Trace

tests = [
      testProperty "nWindows" $ propNWindows
    , testProperty "Randoms windows" $ propRandomWindows
    ]

-- | Tests if the exhaustive list of windows returns the same number of windows
-- as 'nWindows'.
propNWindows :: I.GreyImage -> Bool
propNWindows image =
    W.nWindows (II.originalSize ii) == length (W.windows ii sqii)
  where
    ii = II.integralImage image id
    sqii = II.integralImage image (^2)
    
-- | Tests if the randoms windows are in the exhaustive list of windows.
propRandomWindows :: I.GreyImage -> Bool
propRandomWindows image =
    S.null winRects || go winRects randRects
  where
    ii = II.integralImage image id
    sqii = II.integralImage image (^2)
    winRects = S.fromList $ map W.wRect $ W.windows ii sqii
    randRects = map W.wRect $ W.randomWindows (mkStdGen 1) ii sqii
    
    go remainSet (r:rs) 
        | S.null remainSet = True
        | otherwise        = 
            S.member r winRects && go (S.delete r remainSet) rs