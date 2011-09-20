{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AdaBoost (
    -- * Classes
      TrainingTest (..), Classifier (..)
    -- * Types
    , Weight, StrongClassifier (..)
    -- * Algorithm
    , adaBoost
) where

import Data.Function
import Data.List
import Debug.Trace

type Weight = Double

-- | Represents an instance of a testable item (image ...).
class TrainingTest a where
    isValid :: a -> Bool

-- | Represents an instance of a classifier able to classify
-- a type of tests.
class (Show classifier, Read classifier) => Classifier classifier test where
    check :: classifier -> test -> Bool

-- | A 'StrongClassifier' is a trained container with a set of classifiers.
-- The 'StrongClassifier' can be trained with the 'adaBoost' algorithm.
data StrongClassifier a = StrongClassifier {
      scClassifiers :: [WeakClassifier a]
    } deriving (Show, Read)

-- | A 'WeakClassifier' contain a classifier with an associed weight.
data WeakClassifier a = WeakClassifier {
      wcClassifier :: a
    , wcWeight :: Weight
    } deriving (Show, Read)
    
-- | Each 'StrongClassifier' can be used as a 'Classifier' if the contained
-- 'WeakClassifier' is itself a instance of 'Classifier'.
instance (Classifier weak b) => Classifier (StrongClassifier weak) b where
    check (StrongClassifier cs) test = sumValids > 0
      where
        sumValids = sum $ map validsMap cs
        validsMap (WeakClassifier c w) = if c `check` test then w
                                                           else -w

-- | Train a strong classifier from a weak classifier selector and a set of
-- tests.
-- The selector get a list of tests associated with a weight and return the best
-- weak classifier with an error score, wich is the sum of failed tests.
-- The weak classifier must be able to classify the tests.
adaBoost :: (Classifier a b, TrainingTest b, Show a, Read a) =>
    Int -> [b] -> ([(b, Weight)] -> (a, Weight)) -> StrongClassifier a
adaBoost steps initTests weakSelector =
    StrongClassifier $ take steps $ selectClassifiers initTests'
  where
    (valids, invalids) = partition isValid initTests

    -- Set an initial weight for each test (total weight : 50% valids/invalids)
    initTests' =
        let createTest weight t = (t, weight)
            nValids = fromIntegral (length valids) :: Double
            nInvalids = fromIntegral (length invalids) :: Double
        in map (createTest (1 / (nValids * 2))) valids ++
           map (createTest (1 / (nInvalids * 2))) invalids

    -- One step: select a new classifier
    selectClassifiers tests =
        WeakClassifier c cWeight : selectClassifiers tests'
      where
        (c, cError) = weakSelector tests
        
        cWeight = 0.5 * (log $ (1-cError) / cError)

        -- Reduce the weight of positive tests, increment the weight
        -- of failed tests.
        tests' = normalizeWeights $ flip map tests $ \(t, w) ->
            if c `check` t
               then (t, w * exp (-cWeight))
               else (t, w * exp cWeight)

        normalizeWeights ts =
            let sumWeights = sum $ map snd ts
            in flip map ts $ \(t, w) -> (t, w / sumWeights)