{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Trainer (
    -- * Types & constructors
      TrainingImage (..)
    -- * Weak classifier selector
    , selectHaarClassifier
    -- * Impure utilities
    , train
    ) where

import Control.Parallel.Strategies
import Data.Function
import Data.Int
import Data.List
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, (</>))

import AI.Learning.AdaBoost (adaBoost)
import AI.Learning.Classifier (
      TrainingTest (..), Classifier (..), Weight, Score, classifierScore
    )
import AI.Learning.DecisionStump (
      DecisionStump, DecisionStumpTest (..), trainDecisionStump
    )

import Vision.Haar.Classifier (HaarClassifier (..))
import Vision.Haar.Feature (HaarFeature, features, compute)
import Vision.Haar.Window (Win, win, windowWidth, windowHeight)
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Size (..), Rect (..))

-- | Contains a training image with its 'IntegralImage'.
data TrainingImage = TrainingImage {
      tiWindow :: !Win, tiValid :: !Bool
    }

instance TrainingTest TrainingImage Bool where
    tClass = tiValid

instance Classifier HaarClassifier TrainingImage Bool where
    classifier `cClassScore` image = classifier `cClassScore` tiWindow image

-- | Builds an 'HaarClassifier' which make the best score in classifying the set
-- of tests and weights given.
-- The classifier selection can benefit from parallel computing.
selectHaarClassifier :: [(TrainingImage, Weight)] -> (HaarClassifier, Score)
selectHaarClassifier ts =
    -- Selects the best 'DecisionStump' over all features.
    maximumBy (compare `on` snd) bestClassifiers
  where
    -- Compute the best 'DecisionStump' for each feature on the set of tests,
    -- using parallel computing.
    bestClassifiers =
        let parStrategy = evalTuple2 rseq rseq
        in parMap parStrategy featureStump features 
    
    featureStump f =
        let (stump, score) = trainDecisionStump [
                  (DecisionStumpTest (f `compute` tiWindow t) (tiValid t), w) | 
                  (t, w) <- ts
                ]
        in (HaarClassifier f stump, score)

-- | Trains a strong classifier from directory of tests containing two
-- directories (bad & good).
train :: FilePath -> Int -> FilePath -> IO ()
train directory steps savePath = do
    putStrLn "Loading images ..."
    good <- loadIntegrals True (directory </> "good")
    putStrLn "\tgood/ loaded"
    bad <- loadIntegrals False (directory </> "bad")
    putStrLn "\tbad/ loaded"
    let tests = good ++ bad

    putStrLn $ "Train classifier on "++ show (length tests) ++ " image(s) ..."
    let classifier = adaBoost steps tests selectHaarClassifier
    print classifier
    
    let score = classifierScore classifier tests
    putStrLn $ "Classifier score is " ++ show (score * 100) ++ "%"

    putStrLn "Save classifier ..."
    writeFile savePath $ show classifier
  where
    loadIntegrals isValid = (trainingImages isValid `fmap`) . loadImages

    loadImages dir = do
        paths <- getDirectoryContents $ dir
        mapM (loadImage . (dir </>)) (excludeHidden paths)

    loadImage path = do
        img <- I.load path
        return $ I.resize img $ Size windowWidth windowHeight

    excludeHidden = filter $ ((/=) '.') . head

-- | Accepts a list of images with a boolean indicating if the image is valid.
-- Compute the 'IntegralImage' and initialises a full image 'Win' for each
-- image.
trainingImages :: Bool -> [G.GreyImage] -> [TrainingImage]
trainingImages isValid =
    map trainingImage
  where
    rect = Rect 0 0 windowWidth windowHeight
    trainingImage image =
        let integral = II.integralImage image id
            squaredIntegral = II.integralImage image (^2)
            window = win rect integral squaredIntegral
        in TrainingImage window isValid