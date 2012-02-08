{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AI.Learning.AdaBoost (
    -- * Classes
      TrainingTest (..), Classifier (..)
    -- * Types
    , Weight, StrongClassifier (..)
    -- * Algorithm
    , adaBoost
    ) where

import Data.Function
import Data.List
import qualified Data.Map as M

type Weight = Double

-- | Represents an instance of a testable item (image ...) with a method to gets
-- its class identifier (i.e. 1 and 0 for binary classes).
class TrainingTest t where
    tClass :: t -> Int -- ^ Gives the class identifier of the test

-- | Represents an instance of a classifier able to classify a type of tests
-- for a class of items.
class Classifier cl t where
    -- | Infers the class of the test using the classifier.
    cClass :: cl -> t -> Int

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
instance (Classifier weak t) => Classifier (StrongClassifier weak) t where
    cClass (StrongClassifier cs) test =
        fst $ maximumBy (compare `on` snd) classesScores
      where
        -- Uses a 'Map' to sum weights by classes.
        -- Gives the list of classes with score.
        classesScores = M.toList $ foldl' scoreAcc M.empty cs
        scoreAcc mapAcc (WeakClassifier c w) =
            M.insertWith' (+) (cClass c test) w mapAcc

-- | Trains a strong classifier from a weak classifier selector and a set of
-- tests.
-- The selector gets a list of tests associated with a weight and return the
-- best weak classifier with an error score, wich is the sum of failed tests.
-- The weak classifier must be able to classify the tests.
adaBoost :: (Classifier cl t, TrainingTest t)
         => Int -> [t] ->
            -- | The selector which builds an optimal 'WeakClassifier' for the
            -- set of tests.
            ([(t, Weight)] -> (cl, Weight))
            -> StrongClassifier cl
adaBoost steps initTests weakSelector =
    StrongClassifier $ take steps $ selectClassifiers weakSelector initTests'
  where
    -- Groups the tests per class.
    groupedTests =
        let tClassEq = (==) `on` tClass
            tClassCompare = compare `on` tClass
        in groupBy tClassEq $ sortBy tClassCompare initTests

    -- Sets an initial weight for each test.
    -- Test's weight = 100% / n classes / n tests for this class.
    initTests' =
        let classesWeights = 1.0 / fromIntegral (length groupedTests)
            testsWeight tests = classesWeights / fromIntegral (length tests)
            testsWithWeights tests = map (\t -> (t, testsWeight tests)) tests
        in concatMap testsWithWeights groupedTests
        
-- | One step : selects a new weak classifier, update the weights.
selectClassifiers weakSelector tests =
    WeakClassifier cl cWeight : selectClassifiers weakSelector tests'
  where
    (cl, cError) = weakSelector tests
    
    cWeight = 0.5 * (log $ (1-cError) / cError)

    -- Reduces the weight of positive tests, increment the weight of negative
    -- tests.
    tests' = normalizeWeights $ flip map tests $ \(t, w) ->
        if cClass cl t == tClass t
            then (t, w * exp (-cWeight))
            else (t, w * exp cWeight)

    normalizeWeights ts =
        let sumWeights = sum $ map snd ts
        in flip map ts $ \(t, w) -> (t, w / sumWeights)