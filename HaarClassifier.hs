{-# LANGUAGE MultiParamTypeClasses #-}

module HaarClassifier(
      HaarClassifier (..)
) where

import Data.Int

import AdaBoost (Classifier (..))
import HaarFeatures (HaarFeature, compute)
import qualified IntegralImage as II
import Window (Win)

-- | 'Classifier' using a pseudo-haar feature to check an object
data HaarClassifier = HaarClassifier {
      hcFeature :: HaarFeature
    , hcThreshold :: Int64
    , hcParity :: Bool -- ^ True -> higher/equal threshold, False -> lower.
    } deriving (Show, Read)

instance Classifier HaarClassifier Win where
    (HaarClassifier feature thres parity) `check` window =
        let val = compute feature window
        in if parity then val >= thres
                     else val < thres