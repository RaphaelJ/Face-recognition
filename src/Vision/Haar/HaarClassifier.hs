{-# LANGUAGE MultiParamTypeClasses #-}

module HaarClassifier(
    -- * Types & constructors
      HaarClassifier (..)
    ) where

import Data.Int

import AdaBoost (Classifier (..))
import HaarFeatures (HaarFeature, compute)
import Window (Win)

-- | Weak Haar\'s 'Classifier' using a 'HaarFeature' to check an object.
data HaarClassifier = HaarClassifier {
      hcFeature :: HaarFeature
    , hcThreshold :: Int64
    , hcParity :: Bool -- ^ True -> higher/equal threshold, False -> lower.
    } deriving (Show, Read)

-- | The 'HaarClassifier' is able to classify a part of an image using its
-- iteration window.
instance Classifier HaarClassifier Win where
    cClass (HaarClassifier feature thres parity) window =
        let val = compute feature window
        in if parity
              then fromEnum $ val >= thres
              else fromEnum $ val < thres