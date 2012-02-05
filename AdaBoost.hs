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
import qualified Data.Map as M
import Debug.Trace

type Weight = Double

-- | Represents an instance of a testable item (image ...) with its classes\'
-- type (Bool for binary classes, Int for many classes, ...).
class Eq c => TrainingTest t c where
    tClass :: t -> c -- ^ Gives the class value of the test

-- | Represents an instance of a classifier able to classify a type of tests
-- for a class of items.
class (Show cl, Read cl, Eq c) => Classifier cl t c where
    cClass :: cl -> t -> c -- Infers the class of the test using the classifier

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
-- The class's type must be an instance of 'Ord' for fast seeking using 'Map'
-- The 'StrongClassifier' gives the class with the strongest score.
instance (Classifier weak t c, Ord c)
         => Classifier (StrongClassifier weak) t c where
    cClass (StrongClassifier cs) test =
        fst $ maximumBy (compare `on` snd) classesScores
      where
        -- Uses a 'Map' to sum weights by classes.
        -- Gives the list of classes sorted by score
        -- Gives the list of classes with score
        classesScores = M.toList $ foldl' scoreAcc M.empty cs
        scoreAcc mapAcc (WeakClassifier c w) =
            M.insertWith' (+) (cClass c test) w mapAcc

-- | Train a strong classifier from a weak classifier selector and a set of
-- tests.
-- The selector get a list of tests associated with a weight and return the best
-- weak classifier with an error score, wich is the sum of failed tests.
-- The weak classifier must be able to classify the tests.
adaBoost :: (Classifier cl t c, TrainingTest t c, Show cl, Read cl, Ord c)
         => Int -> [t] -> ([(t, Weight)] -> (cl, Weight)) -> StrongClassifier cl
adaBoost steps initTests weakSelector =
    StrongClassifier $ take steps $ selectClassifiers initTests'
  where
    -- Group tests by class
    classes =
        groupBy ((==) `on` tClass) $ sortBy (compare `on` tClass) $ initTests

    -- Set an initial weight for each test
    -- Test's weight = 100% / n classes / n test for this class
    initTests' =
        let createTest weight t = (t, weight)
            classesWeights = 1.0 / fromIntegral (length classes) :: Double
            testWeight tests = classesWeights / fromIntegral (length tests)
            mapTests tests = map (createTest (testWeight tests)) tests
        in concatMap (mapTests) classes

    -- One step: select a new classifier
    selectClassifiers tests =
        WeakClassifier cl cWeight : selectClassifiers tests'
      where
        (cl, cError) = weakSelector tests
        
        cWeight = 0.5 * (log $ (1-cError) / cError)

        -- Reduce the weight of positive tests, increment the weight
        -- of negative tests.
        tests' = normalizeWeights $ flip map tests $ \(t, w) ->
            if cClass cl t == tClass t
               then (t, w * exp (-cWeight))
               else (t, w * exp cWeight)

        normalizeWeights ts =
            let sumWeights = sum $ map snd ts
            in flip map ts $ \(t, w) -> (t, w / sumWeights)