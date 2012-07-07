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
adaBoost :: (Classifier c t cl, TrainingTest t cl, Ord cl, Show cl)
         => Int -> [t]
         -- | The selector which builds an optimal 'WeakClassifier' for the
         -- set of tests.
         -> ([(t, Weight)] -> (c, Score))
         -> StrongClassifier c
adaBoost steps initTests weakSelector =
    StrongClassifier cs (sum $ map snd cs)
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
    
    cs = take steps $ selectClassifiers weakSelector initTests'

-- | One step : selects a new weak classifier, update the weights.
selectClassifiers weakSelector tests =
    (c, cWeight) : selectClassifiers weakSelector tests'
  where
    (c, cScore) = weakSelector tests
    
    cWeight = 0.5 * (log $ cScore / (1.0 - cScore))

    -- Reduces the weight of positive tests, increment the weight of negative
    -- tests.
    tests' = normalizeWeights $ flip map tests $ \(t, w) ->
        if c `cClass` t == tClass t
            then (t, w * exp (-cWeight))
            else (t, w * exp cWeight)

    normalizeWeights ts =
        let sumWeights = sum $ map snd ts
        in flip map ts $ \(t, w) -> (t, w / sumWeights)