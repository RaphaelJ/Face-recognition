{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Haar.Classifier (
    -- * Types & constructors
      HaarClassifier (..)
    ) where

import Data.Int

import AI.Learning.AdaBoost (Classifier (..))
import Vision.Haar.Feature (HaarFeature, compute)
import Vision.Haar.Window (Win)

-- | Weak Haar\'s 'Classifier' using a 'HaarFeature' to check an object.
data HaarClassifier = HaarClassifier {
      hcFeature :: !HaarFeature
    , hcThreshold :: !Int64
    , hcParity :: !Bool -- ^ True -> higher/equal threshold, False -> lower.
    } deriving (Show, Read)

-- | The 'HaarClassifier' is able to classify a part of an image using its
-- iteration window.
instance Classifier HaarClassifier Win Bool where
    cClass (HaarClassifier feature thres parity) window =
        let val = compute feature window
        in if parity
              then val >= thres
              else val < thres