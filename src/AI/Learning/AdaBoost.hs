module AI.Learning.AdaBoost (
    -- * Algorithm
      adaBoost
    ) where

import Data.Function
import Data.List

import AI.Learning.Classifier (
      Classifier (..), TrainingTest (..), Weight, Score, StrongClassifier (..)
    )

-- | Trains a strong classifier from a weak classifier selector and a set of
-- tests.
-- The selector gets a list of tests associated with a weight and return the
-- best weak classifier with an error score, wich is the sum of failed tests.
-- The weak classifier must be able to classify the tests.
adaBoost :: (Classifier c t cl, TrainingTest t cl, Ord cl, Show cl) =>
         -- | Stop the training after a number of sub-classifiers or after a
         -- false positive score is reached.
            Either Int Score
         -> [t]
         -- | The selector which builds an optimal 'WeakClassifier' for the
         -- set of tests.
         -> ([(t, Weight)] -> (c, Score))
         -> StrongClassifier c
adaBoost stopCond initTrainingSet weakSelector =
    StrongClassifier cs (sum $ map snd cs)
  where
    -- Sets an initial weight for each training test.
    -- Test's weight = 100% / n classes / n tests for this class.
    initTrainingSet' = concatMap testsWeights groupedTests

    classesWeights = 1.0 / fromIntegral (length groupedTests)
    testsWeights tests =
        let weight = classesWeights / fromIntegral (length tests)
        in map (\t -> (t, weight)) tests

    -- Groups the tests per class.
    groupedTests = groupBy tClassEq $ sortBy tClassCompare initTrainingSet

    tClassEq = (==) `on` tClass
    tClassCompare = compare `on` tClass

    cs = 
        let cs' = selectClassifiers initTrainingSet' weakSelector
        in case stopCond of
                Left steps -> take steps cs'
                Right falsePositiveScore -> undefined

-- | One step : selects a new weak classifier, update the weights.
selectClassifiers trainingSet weakSelector =
    (c, cWeight) : selectClassifiers trainingSet' weakSelector
  where
    (c, cScore) = weakSelector trainingSet
    
    cWeight = 0.5 * (log $ cScore / (1.0 - cScore))

    -- Reduces the weight of positive tests, increment the weight of negative
    -- tests.
    trainingSet' = normalizeWeights $ flip map trainingSet $ \(t, w) ->
        if c `cClass` t == tClass t
            then (t, w * exp (-cWeight))
            else (t, w * exp cWeight)

    normalizeWeights ts =
        let sumWeights = sum $ map snd ts
        in flip map ts $ \(t, w) -> (t, w / sumWeights)