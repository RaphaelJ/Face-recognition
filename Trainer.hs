{-# LANGUAGE MultiParamTypeClasses #-}

module Trainer(
      train, selectHaarClassifier
) where

import Control.Parallel.Strategies
import Data.Function
import Data.List
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>))

import AdaBoost (
      TrainingTest (..), Classifier (..)
    , Weight, StrongClassifier, adaBoost
    )
import GreyImage (GreyImage, load)
import HaarClassifier (HaarClassifier (..))
import HaarFeatures (HaarFeature, features, compute)
import IntegralImage (computeIntegralImage)
import Primitives
import Window (Win, win, windowWidth, windowHeight)

data TrainingImage = TrainingImage {
      tiWindow :: Win
    , tiValid :: Bool
    }

instance TrainingTest TrainingImage where
    isValid = tiValid

instance Classifier HaarClassifier TrainingImage where
    classifier `check` image = classifier `check` (tiWindow image)

-- | Train a strong classifier from directory of tests
-- containing two directories (bad & good).
train directory steps savePath = do
    putStrLn "Loading images ..."
    good <- loadIntegral (directory </> "good") True
    putStrLn "\tgood/ loaded"
    bad <- loadIntegral (directory </> "bad") False
    putStrLn "\tbad/ loaded"
    let tests = good ++ bad

    putStrLn "Train classifier ..."
    let classifier = adaBoost steps tests selectHaarClassifier
    print classifier

    putStrLn "Save classifier ..."
    writeFile savePath $ show classifier
    
  where
    loadIntegral dir valid =
        fmap (trainingImages valid) $ loadImages dir
     
    loadImages dir = do
        paths <- getDirectoryContents $ dir
        mapM (loadImage . (dir </>)) (excludeHidden paths)
        
    loadImage path =
        load path $ Just $ Size windowWidth windowHeight
        
    excludeHidden = filter $ ((/=) '.') . head

-- | Accepts a list of images with a boolean indicating if the image is valid.
-- Compute the 'IntegralImage' and initialise a full image 'Win' for each image.
trainingImages :: Bool -> [GreyImage] -> [TrainingImage]
trainingImages valid =
    map $ \image ->
        let int = computeIntegralImage image id
            squaredInt = computeIntegralImage image (^2)
            window = win rect int squaredInt
        in TrainingImage window valid
  where
    rect = (Rect 0 0 windowWidth windowHeight)

-- | Select the best 'HaarClassifier' with the set of tests and weights.
selectHaarClassifier :: [(TrainingImage, Weight)] -> (HaarClassifier, Weight)
selectHaarClassifier tests =
    -- Select the best classifier over all features
    minimumBy (compare `on` snd) $ parMap parStrategy bestClassifier features
  where
    -- List all possibles classifiers associated with theirs error for a
    -- feature and the set of tests.
    featureClassifiers feature =
        -- The first computed classifier will give « False » for each test,
        -- so its error score is the weight of valid tests.
        fst $ foldl' (\(cs, trueScore) (value, valid, weight) ->
            let trueScore' = if valid
                    then trueScore - weight
                    else trueScore + weight
                falseScore' = 1.0 - trueScore'

                c1 = (HaarClassifier feature value True, trueScore')
                c2 = (HaarClassifier feature value False, falseScore')
            in (c1 : c2 : cs, trueScore')
        ) ([], weightValid) (featureValuesSorted feature tests)

    -- Select the best classifier for a feature
    bestClassifier = minimumBy (compare `on` snd) . featureClassifiers

    weightValid = sum $ map snd $ filter (isValid . fst) tests

    parStrategy = evalTuple2 rseq rseq

-- | Compute all feature's values with a set of tests, sorted.
-- Keep the test validity and its weight.
featureValuesSorted feature tests =
    sortBy (compare `on` value) $ flip map tests $ \(t, w) ->
        (compute feature (tiWindow t), isValid t, w)
  where
    value (v, _, _) = v