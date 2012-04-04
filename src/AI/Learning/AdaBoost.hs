{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module AI.Learning.AdaBoost (
    -- * Classes
      TrainingTest (..), Classifier (..)
    -- * Types
    , Weight, StrongClassifier (..)
    -- * Algorithm
    , adaBoost
    ) where

import Debug.Trace
    
import Data.Function
import Data.List
import qualified Data.Map as M

type Weight = Double

-- | Represents an instance of a testable item (image ...) with a method to gets
-- its class identifier (i.e. Bool for binary classes, ...).
class TrainingTest t cl | t -> cl where
    -- | Gives the class identifier of the test.
    tClass :: t -> cl

-- | Represents an instance of a classifier able to classify a type of tests
-- for a class of items.
--
-- Minimal complete definition: 'cClassScore'.
class Classifier c t cl | c t -> cl where
    -- | Infers the class of the test using the classifier.
    cClass :: c -> t -> cl

    -- | Infers the class of the test using the classifier with a score ([0;1]
    -- with @1@ for sure, @0@ for unlikely).
    cClassScore :: c -> t -> (cl, Weight)

    classifier `cClass` test = fst $ classifier `cClassScore` test

-- | A 'StrongClassifier' is a trained container with a set of classifiers.
-- The 'StrongClassifier' can be trained with the 'adaBoost' algorithm.
data StrongClassifier a = StrongClassifier {
      scClassifiers :: [WeakClassifier a]
    } deriving (Show, Read)

-- | A 'WeakClassifier' contains a classifier with an associed weight.
data WeakClassifier a = WeakClassifier {
      wcClassifier :: a
    , wcWeight :: Weight
    } deriving (Show, Read)
    
-- | Each 'StrongClassifier' can be used as a 'Classifier' if the contained
-- 'WeakClassifier' is itself an instance of 'Classifier'.
-- The 'StrongClassifier' will give the class with the strongest score.
instance (Classifier weak t cl, Ord cl)
         => Classifier (StrongClassifier weak) t cl where
    StrongClassifier cs `cClassScore` test =
        maximumBy (compare `on` snd) classesScores
      where
        -- Uses a 'Map' to sum weights by classes.
        -- Gives the list of classes with score.
        classesScores = M.toList $ foldl' step M.empty cs
        step acc (WeakClassifier c w) =
            let (cl, score) = c `cClassScore` test
            in M.insertWith' (+) cl (w * score) acc

-- | Trains a strong classifier from a weak classifier selector and a set of
-- tests.
-- The selector gets a list of tests associated with a weight and return the
-- best weak classifier with an error score, wich is the sum of failed tests.
-- The weak classifier must be able to classify the tests.
adaBoost :: (Classifier c t cl, TrainingTest t cl, Ord cl, Show cl)
         => Int -> [t]
            -- | The selector which builds an optimal 'WeakClassifier' for the
            -- set of tests.
            -> ([(t, Weight)] -> (c, Weight))
            -> StrongClassifier c
adaBoost steps initTests weakSelector =
    StrongClassifier $ take steps $ selectClassifiers weakSelector initTests'
  where
    -- Sets an initial weight for each test.
    -- Test's weight = 100% / n classes / n tests for this class.
    initTests' = concatMap testsWeights groupedTests

    classesWeights = 1.0 / fromIntegral (length groupedTests)
    testsWeights tests =
        let weight = classesWeights / fromIntegral (length tests)
        in map (\t -> (t, weight)) tests

    -- Groups the tests per class.
    groupedTests = groupBy tClassEq $ sortBy tClassCompare initTests

    tClassEq = (==) `on` tClass
    tClassCompare = compare `on` tClass

-- | One step : selects a new weak classifier, update the weights.
selectClassifiers weakSelector tests =
    WeakClassifier c cWeight : selectClassifiers weakSelector tests'
  where
    (c, cError) = weakSelector tests
    
    cWeight = 0.5 * (log $ (1.0-cError) / cError)

    -- Reduces the weight of positive tests, increment the weight of negative
    -- tests.
    tests' = normalizeWeights $ flip map tests $ \(t, w) ->
        if c `cClass` t == tClass t
            then (t, w * exp (-cWeight))
            else (t, w * exp cWeight)

    normalizeWeights ts =
        let sumWeights = sum $ map snd ts
        in flip map ts $ \(t, w) -> (t, w / sumWeights)