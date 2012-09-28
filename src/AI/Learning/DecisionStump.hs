{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AI.Learning.DecisionStump (
    -- * Types
      DecisionStump (..)
    -- * Functions
    , trainDecisionStump
    ) where

import Data.Function
import Data.List

import AI.Learning.Classifier (
      Classifier (..), Weight, Score, TrainingTest (..)
    )

-- | Data representation of a 'DecisionStump' classifier
data DecisionStump a = DecisionStump {
      dsThreshold :: !a
    , dsParity :: !Bool
    -- ^ True: higher/equal than threshold, False: lower.
    } deriving (Show, Read, Eq)

instance Ord a => Classifier (DecisionStump a) a Bool where
    DecisionStump thres parity `cClassScore` val =
        let valid = if parity
                       then val >= thres
                       else val < thres
        in (valid, 1.0)
    {-# INLINE cClassScore #-}

-- | Select the best threshold for the 'DecisionStump'. Positives tests gets a
-- positive weight and negative tests a negative weight. Returns the stump with
-- its error score.
trainDecisionStump :: Ord a => [(TrainingTest a Bool, Weight)]
                   -> (DecisionStump a, Score)
trainDecisionStump ts =
    -- Selects the best classifier over all features.
    maximumBy score stumps
  where
    -- | Lists all possibles stumps configurations associated with theirs
    -- error for the set of tests.
    stumps =
        -- The first computed stump will give 'True' for each test, so its
        -- score is the weight of valid tests.
        fst $ foldl' step ([], weightValid) values

    step (cs, trueScore) (value, weights) =
        let c1 = (DecisionStump value True, trueScore)
            falseScore = 1.0 - trueScore
            c2 = (DecisionStump value False, falseScore)

            trueScore' = trueScore - weights
        in (c1 : c2 : cs, trueScore')

    -- | Sums the weight of all valid tests.
    weightValid = sum [ w | (t, w) <- ts, tClass t ]

    values = groupValues ts

    score = compare `on` snd
{-# INLINABLE trainDecisionStump #-} -- Inline to specialise the function

-- | Sorts and then groups values. Returns each distinct value with the sum of
-- all of its weights.
groupValues :: Ord a => [(TrainingTest a Bool, Weight)] -> [(a, Weight)]
groupValues =
    groups . sortBy (compare `on` (tTest . fst))
  where
    -- | Groups the same consecutive values and sums their weights.
    groups []      = []
    groups (t:ts) =
        let value = tTest $ fst t
            (same, ts') = span ((== value) . tTest . fst) ts
            groupWeight = signedWeight t + sum (map signedWeight same)
        in (value, groupWeight) : groups ts'

    signedWeight (t, w) = if tClass t then w else -w
{-# INLINE groupValues #-}