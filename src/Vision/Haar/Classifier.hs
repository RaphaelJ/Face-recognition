{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Contains everything to train and use a 'HaarClassifier'.
-- A 'HaarClassifier' is a simple 'DecisionStump' which classifies a windows
-- when a feature exceeds a certain threshold.
module Vision.Haar.Classifier (
    -- * Types & constructors
      HaarClassifier (..), TrainingImage (..)
    -- * Functions
    , trainHaarClassifier
    ) where

import Control.Parallel.Strategies
import Data.Function
import Data.Int
import Data.List

import GHC.Conc (numCapabilities)

import AI.Learning.Classifier (
      TrainingTest (..), Classifier (..), Weight, Score
    )
import AI.Learning.DecisionStump (
      DecisionStump, DecisionStumpTest (..), trainDecisionStump
    )

import Vision.Haar.Feature (HaarFeature, features, compute)
import Vision.Haar.Window (Win)

-- | Weak Haar\'s 'Classifier' using a 'HaarFeature' to check an object.
data HaarClassifier = HaarClassifier {
      hcFeature :: !HaarFeature, hcStump :: !(DecisionStump Int64)
    } deriving (Show, Read)

-- | Contains a training image with its 'IntegralImage'.
data TrainingImage = TrainingImage {
      tiWindow :: !Win, tiValid :: !Bool
    }

-- | The 'HaarClassifier' is able to classify an iteration window from an image.
instance Classifier HaarClassifier Win Bool where
    HaarClassifier feature stump `cClassScore` window =
        let !value = feature `compute` window
        in stump `cClassScore` value
    {-# INLINE cClassScore #-}

instance TrainingTest TrainingImage Bool where
    tClass = tiValid
    {-# INLINE tClass #-}

instance Classifier HaarClassifier TrainingImage Bool where
    classifier `cClassScore` image = classifier `cClassScore` tiWindow image
    {-# INLINE cClassScore #-}

-- | Defines how the features list must be divided so only a chunk is running
-- on each core when training using parallel computing.
chunksSize :: Int
chunksSize = length features `quot` numCapabilities

-- | Builds an 'HaarClassifier' which make the best score in classifying the set
-- of tests and weights given.
-- The classifier selection can benefit from parallel computing.
trainHaarClassifier :: [(TrainingImage, Weight)] -> (HaarClassifier, Score)
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
        in map featureStump features `using` parListChunk chunksSize strategy

    -- Trains the best 'DecisionStump' for the feature and the set of tests.
    featureStump f =
        let (stump, score) = trainDecisionStump [
                  (DecisionStumpTest (f `compute` tiWindow t) (tiValid t), w)
                | (t, w) <- ts
                ]
        in (HaarClassifier f stump, score)