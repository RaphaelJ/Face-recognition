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

import AI.Learning.AdaBoost (TrainingTest (..), Classifier (..), Weight)

data DecisionStump a = DecisionStump {
      dsThreshold :: !a
    , dsParity :: !Bool -- ^ True: higher/equal than threshold, False: lower.
    } deriving (Show, Read)
    
data DecisionStumpTest a = DecisionStumpTest {
      dstValue :: !a
    , dstValid :: !Bool
    } deriving (Show, Read)
    
instance Ord a => Classifier (DecisionStump a) a Bool where
    DecisionStump thres parity `cClassScore` val =
        let valid = if parity
                       then val >= thres
                       else val < thres
        in (valid, 1.0)
    {-# INLINE cClassScore #-}
    
instance Ord a => TrainingTest (DecisionStumpTest a) Bool where
    tClass = dstValid
    {-# INLINE tClass #-}
    
instance Ord a => Classifier (DecisionStump a) (DecisionStumpTest a) Bool where
    stump `cClassScore` test = stump `cClassScore` (dstValue test)
    {-# INLINE cClassScore #-}

-- | Select the best threshold for the 'DecisionStump'. Positives tests gets a
-- positive weight and negative tests a negative weight. Returns the stump with
-- its error score.
trainDecisionStump :: Ord a => [(DecisionStumpTest a, Weight)] 
                         -> (DecisionStump a, Weight)
trainDecisionStump ts =
    -- Selects the best classifier over all features.
    minimumBy weight stumps
  where
    -- Lists all possibles stumps configurations associated with theirs
    -- error for the set of tests.
    stumps =
        -- The first computed stump will give 'True' for each test, so its
        -- error score is the weight of invalid tests.
        fst $ foldl' step ([], weightInvalid) (groupedValues testValues)
    
    step (cs, trueError) (value, weights) =
        let c1 = (DecisionStump value True, trueError)
            falseError = 1.0 - trueError
            c2 = (DecisionStump value False, falseError)
            
            trueError' = trueError + (sum weights)
        in (c1 : c2 : cs, trueError')

    -- Sums the weight of all non valid tests.
    weightInvalid = - sum [ w | (t, w) <- ts, not $ dstValid t ]
    
    testValues = map (\(t, w) -> (dstValue t, w)) ts
    
    weight = compare `on` snd
    
-- | Sorts and groups values. Returns each distinct value with a list of 
-- weights.
groupedValues :: Ord a => [(a, Weight)] -> [(a, [Weight])]
groupedValues =
    groupByValue . sortBy (compare `on` fst)
  where
    -- Groups the same values in a tuple containing the value and the list of
    -- weights.
    groupByValue =
        map (\((v, w) : xs) -> (v, w : map snd xs)) . groupBy ((==) `on` fst)