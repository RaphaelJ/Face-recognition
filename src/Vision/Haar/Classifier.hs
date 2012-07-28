{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vision.Haar.Classifier (
    -- * Types & constructors
      HaarClassifier (..)
    ) where

import Data.Int

import AI.Learning.Classifier (Classifier (..))
import AI.Learning.DecisionStump (DecisionStump (..))

import Vision.Haar.Feature (HaarFeature, compute)
import Vision.Haar.Window (Win)

-- | Weak Haar\'s 'Classifier' using a 'HaarFeature' to check an object.
data HaarClassifier = HaarClassifier {
      hcFeature :: !HaarFeature, hcStump :: !(DecisionStump Int64)
    } deriving (Show, Read)

-- | The 'HaarClassifier' is able to classify a iteration window from an image.
instance Classifier HaarClassifier Win Bool where
    HaarClassifier feature stump `cClassScore` window =
        let !value = feature `compute` window
        in stump `cClassScore` value
    {-# INLINE cClassScore #-}