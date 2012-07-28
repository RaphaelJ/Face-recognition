{-# LANGUAGE MultiParamTypeClasses #-}

module Tests.AI.Learning.AdaBoost (
      tests
    ) where

import Data.Function
import Data.List
import Data.Ratio

import Test.Framework.Providers.HUnit
import Test.HUnit

import AI.Learning.AdaBoost 
import AI.Learning.Classifier
import AI.Learning.DecisionStump

tests = [
      testCase "AdaBoost performance" caseAdaBoost
    ]

boostingDataSet = "Tests/AI/Learning/bupa.data"

-- | Tests the performance of the 'AdaBoost' algorithm using the BUPA dataset
-- (http://www.cs.huji.ac.il/~shais/datasets/ClassificationDatasets.html) 
-- described at http://www.cs.huji.ac.il/~shais/datasets/bupa/bupa.names
caseAdaBoost = do
    tests <- read `fmap` readFile boostingDataSet
    -- Trains on the first 75% values and test the score on the 25% values which
    -- remains.
    let (training, testing) = splitTests (75 % 100) tests 
    let classifier = adaBoost (Left 200) training trainBupaClassifier
    let score = classifierScore classifier testing
    putStrLn $ "AdaBoost performance is " ++ show (score * 100) ++ "%"
    
    assertBool "AdaBoost performance is less than 66%" (score > 0.65)

data BupaTestCase = BupaTestCase {
      bupaMcv :: Int, bupaAlkphos :: Int, bupaSgpt :: Int, bupaSgot :: Int
    , bupaGammagt :: Int, bupaDrinks :: Int, bupaSelector :: Bool
    } deriving (Show, Read, Eq)
    
data BupaVariable = BupaMcv | BupaAlkphos | BupaSgpt | BupaSgot | BupaGammagt 
                  | BupaDrinks
     deriving (Show, Read, Eq)
    
-- | Each weak 'BupaClassifier' is a 'DecisionStump' on a specified variable
-- from the tests.
data BupaClassifier = BupaClassifier {
      bupaVar :: BupaVariable, bupaDecisionStump :: DecisionStump Int
    } deriving (Show, Read, Eq)

instance TrainingTest BupaTestCase Bool where
    tClass = bupaSelector
    {-# INLINE tClass #-}
    
instance Classifier BupaClassifier BupaTestCase Bool where
    BupaClassifier variable stump `cClassScore` test =
        case variable of 
             BupaMcv     -> stump `cClassScore` bupaMcv test
             BupaAlkphos -> stump `cClassScore` bupaAlkphos test
             BupaSgpt    -> stump `cClassScore` bupaSgpt test
             BupaSgot    -> stump `cClassScore` bupaSgot test
             BupaGammagt -> stump `cClassScore` bupaGammagt test
             BupaDrinks  -> stump `cClassScore` bupaDrinks test
    {-# INLINABLE cClassScore #-}

-- | Train the Bupa classifier by selecting the best 'DecisionStump' for the
-- set of variables.
trainBupaClassifier :: [(BupaTestCase, Weight)] -> (BupaClassifier, Score)
trainBupaClassifier ts =
    maximumBy (compare `on` snd) [
      trainVar BupaMcv bupaMcv, trainVar BupaAlkphos bupaAlkphos
    , trainVar BupaSgpt bupaSgpt, trainVar BupaSgot bupaSgot
    , trainVar BupaGammagt bupaGammagt, trainVar BupaDrinks bupaDrinks
    ]
  where
    trainVar :: BupaVariable -> (BupaTestCase -> Int) -> (BupaClassifier, Score)
    trainVar variable getter =
        let (stump, score) = trainDecisionStump Nothing $ applyGetter getter ts
        in (BupaClassifier variable stump, score)
    
    applyGetter getter = map $ \(t, w) -> 
        (DecisionStumpTest (getter t) (bupaSelector t), w)