{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Detector (
    -- * Functions
      detect, groupRectangles
    ) where

import Control.Monad.State
import Data.Function
import Data.List
import Data.Ratio

import AI.Learning.Classifier (Score, cClassScore)

import Vision.Haar.Cascade (HaarCascade)
import Vision.Haar.Window (wRect, windows, Win (..))
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Rect (..))

-- | Detects all positive matches inside the image using a strong 'HaarCascade'.
detect :: HaarCascade -> G.GreyImage -> [(Rect, Score)]
detect cascade image =
    let integral = II.integralImage image id
        squaredIintegral = II.integralImage image (^(2 :: Int))
        valids = [ (r, s) | !w <- windows integral squaredIintegral
            , let r = wRect w, let (!v, s) = cascade `cClassScore` w, v
            ]
    in {-groupRectangles -}valids

-- | Groups overlapping rectangles. Keeps the rectangle with the largest 
-- score for each group.
groupRectangles :: [(Rect, Score)] -> [(Rect, Score)]
groupRectangles []     = []
groupRectangles (r:rs) =
    -- Execute a deep first search on each remaining rectangle.
    let (groupLeader, notConnected) = runState (findConnected r) rs
    in groupLeader : groupRectangles notConnected

-- | Execute a deep first search to find all the linked rectangles with the
-- rectangle given in argument. The 'State' monad keeps the rectangles which are
-- still not connected. Keeps the rectangle with the highest score as the leader
-- of the group.
findConnected :: (Rect, Score) -> State [(Rect, Score)] (Rect, Score)
findConnected r = do 
    rs <- get
    let (connected, notConnected) = partition (overlap (fst r) . fst) rs
    put notConnected
    connected' <- mapM findConnected connected -- Find nested connected
    let groupLeader = maximumBy (compare `on` snd) (r : connected')

    return $! groupLeader
  where
    -- | Groups two rectangles if they share more than 50% of their surfaces.
    minOverlap = 0.5 :: Rational
    overlap (Rect x y w h) (Rect x' y' w' h') =
        let x1 = max x x'
            y1 = max y y'
            x2 = min (x + w) (x' + w')
            y2 = min (y + h) (y' + h')
            minS = min (w * h) (w' * h')
            overlapS = (x2 - x1) * (y2 - y1)
        in x2 > x1 && y2 > y1 && (integer overlapS % integer minS) >= minOverlap

integer :: Integral a => a -> Integer
integer = fromIntegral