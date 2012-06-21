module Tests.AI.Learning.AdaBoost (
      tests
    ) where

import Data.List
import Data.List.Split

import Test.Framework.Providers.HUnit
import Test.HUnit

import AI.Learning.AdaBoost 

tests = [
      testCase "AdaBoost performances" caseAdaBoost
    ]

boostingDataSet = "Tests/AI/Learning/bupa.data"

data BupaTestCase = BupaTestCase {
      bupaMcv :: Int, bupaAlkphos :: Int, bupaSgpt :: Int, bupaSgot :: Int
    , bupaGammagt :: Int, bupaDrinks :: Int, bupaSelector :: Bool
    } deriving (Show, Read, Eq)
    
-- | Tests the performances of the 'AdaBoost' algorithm using the BUPA dataset
-- (http://www.cs.huji.ac.il/~shais/datasets/ClassificationDatasets.html) 
-- described at http://www.cs.huji.ac.il/~shais/datasets/bupa/bupa.names
caseAdaBoost = do
    content <- read `fmap` readFile boostingDataSet
    let (training, testing) = splitAt (length content * 100 `div` 75) content
    assertBool "OK" True