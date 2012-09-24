{-# LANGUAGE MultiParamTypeClasses #-}

module Tests.AI.Learning.AdaBoost (
      tests
    ) where

import Data.Function
import Data.List

import Test.Framework.Providers.HUnit
import Test.HUnit

import AI.Learning.AdaBoost
import AI.Learning.Classifier
import AI.Learning.DecisionStump

data BupaCase = BupaCase {
      bupaMcv :: Int, bupaAlkphos :: Int, bupaSgpt :: Int, bupaSgot :: Int
    , bupaGammagt :: Int, bupaDrinks :: Int
    } deriving (Show, Read, Eq)

data BupaVariable = BupaMcv | BupaAlkphos | BupaSgpt | BupaSgot | BupaGammagt
                  | BupaDrinks
    deriving (Show, Read, Eq)

-- | Each weak 'BupaClassifier' is a 'DecisionStump' on a specified variable
-- from the tests.
data BupaClassifier = BupaClassifier {
      bupaVar :: BupaVariable, bupaDecisionStump :: DecisionStump Int
    } deriving (Show, Read, Eq)

instance Classifier BupaClassifier BupaCase Bool where
    BupaClassifier variable stump `cClassScore` bupaCase =
        case variable of
             BupaMcv     -> stump `cClassScore` bupaMcv bupaCase
             BupaAlkphos -> stump `cClassScore` bupaAlkphos bupaCase
             BupaSgpt    -> stump `cClassScore` bupaSgpt bupaCase
             BupaSgot    -> stump `cClassScore` bupaSgot bupaCase
             BupaGammagt -> stump `cClassScore` bupaGammagt bupaCase
             BupaDrinks  -> stump `cClassScore` bupaDrinks bupaCase
    {-# INLINABLE cClassScore #-}

tests = [
      testCase "AdaBoost performance" caseAdaBoost
    ]

boostingDataSet = "Tests/AI/Learning/bupa.data"

-- | Tests the performance of the 'AdaBoost' algorithm using the BUPA dataset
-- (http://www.cs.huji.ac.il/~shais/datasets/ClassificationDatasets.html)
-- described at http://www.cs.huji.ac.il/~shais/datasets/bupa/bupa.names
caseAdaBoost :: IO ()
caseAdaBoost = do
    dataFile <- readFile boostingDataSet
    let bupaData = [ TrainingTest bupaCase valid
            | (bupaCase, valid) <- read dataFile :: (BupaCase, Bool)
            ]

    -- Trains on the first 75% values and test the score on the 25% values which
    -- remains.
    let (training, testing) = splitTests 0.75 bupaData
    let classifier = adaBoost training trainBupaClassifier !! 200
    let score = classifierScore classifier testing
    putStrLn $ "AdaBoost performance is " ++ show (score * 100) ++ "%"

    assertBool "AdaBoost performance is less than 70%" (score > 0.70)

-- | Train the Bupa classifier by selecting the best 'DecisionStump' for the
-- set of variables.
trainBupaClassifier :: [(TrainingTest BupaCase Bool, Weight)]
                    -> (BupaClassifier, Score)
trainBupaClassifier ts =
    maximumBy (compare `on` snd) [
      trainVar BupaMcv bupaMcv, trainVar BupaAlkphos bupaAlkphos
    , trainVar BupaSgpt bupaSgpt, trainVar BupaSgot bupaSgot
    , trainVar BupaGammagt bupaGammagt, trainVar BupaDrinks bupaDrinks
    ]
  where
    trainVar :: BupaVariable -> (BupaCase -> Int) -> (BupaClassifier, Score)
    trainVar variable getter =
        let (stump, score) = trainDecisionStump $ applyGetter getter
        in (BupaClassifier variable stump, score)

    applyGetter getter = [ (TrainingTest (getter $ tTest t) (tClass t), w)
        | (t, w) <- ts
        ]