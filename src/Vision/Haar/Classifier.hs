{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Contains everything to train and use a 'HaarClassifier'.
-- A 'HaarClassifier' is a simple 'DecisionStump' which classifies a windows
-- when a feature exceeds a certain threshold.
module Vision.Haar.Classifier (
    -- * Types & constructors
      HaarClassifier (..)
    -- * Functions
    , trainHaarClassifier
    ) where

import Control.Parallel.Strategies
import Data.Function
import Data.Int
import Data.List

import AI.Learning.Classifier (
      TrainingTest (..), Classifier (..), Weight, Score
    )
import AI.Learning.DecisionStump (
      DecisionStump, trainDecisionStump
    )

import Vision.Haar.Feature (HaarFeature, features, compute)
import Vision.Haar.Window (Win)

-- | Weak Haar\'s 'Classifier' using a 'HaarFeature' to check an object.
data HaarClassifier = HaarClassifier {
      hcFeature :: !HaarFeature, hcStump :: !(DecisionStump Int64)
    } deriving (Show, Read)

-- | The 'HaarClassifier' is able to classify an iteration window from an image.
instance Classifier HaarClassifier Win Bool where
    HaarClassifier feature stump `cClassScore` window =
        let !value = feature `compute` window
        in stump `cClassScore` value
    {-# INLINE cClassScore #-}

{-# SPECIALIZE trainDecisionStump :: [(TrainingTest Int64 Bool, Weight)]
                                  -> (DecisionStump Int64, Score) #-}

-- | Builds an 'HaarClassifier' which make the best score in classifying the set
-- of tests and weights given.
-- The classifier selection can benefit from parallel computing.
trainHaarClassifier :: [(TrainingTest Win Bool, Weight)]
                    -> (HaarClassifier, Score)
trainHaarClassifier ts =
    -- Selects the best 'DecisionStump' over all features.
    maximumBy (compare `on` snd) bestClassifiers
  where
    -- Compute the best 'DecisionStump' for each feature on the set of tests,
    -- using parallel computing.
    bestClassifiers =
        let strategy = evalTuple2 rseq rseq
           -- parMap will cause a space leak because each feature will be
           -- evaluated at the same time.
        in map featureStump features `using` parListChunk 1000 strategy

    -- Trains the best 'DecisionStump' for one feature and the set of tests.
    featureStump f =
        let (stump, score) = trainDecisionStump [
                  (TrainingTest (f `compute` tTest t) (tClass t), w)
                | (t, w) <- ts
                ]
        in (HaarClassifier f stump, score)