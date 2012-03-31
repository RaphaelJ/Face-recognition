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

import Debug.Trace

import Control.Parallel.Strategies
import Data.Function
import Data.Int
import Data.List
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, (</>))

import AI.Learning.AdaBoost (
      TrainingTest (..), Classifier (..)
    , Weight, StrongClassifier, adaBoost
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
      tiWindow :: !Win
    , tiValid :: !Bool
    }

instance TrainingTest TrainingImage Bool where
    tClass = tiValid

instance Classifier HaarClassifier TrainingImage Bool where
    classifier `cClass` image = classifier `cClass` tiWindow image

-- | Builds an 'HaarClassifier' which make the best score in classifying the set
-- of tests and weights given.
-- The classifier selection can benefit from parallel computing.
selectHaarClassifier :: [(TrainingImage, Weight)] -> (HaarClassifier, Weight)
selectHaarClassifier tests =
    -- Selects the best classifier over all features.
    let m = minimumBy weight bestClassifiers
    in trace (show $ errorLevel m) m
  where
    -- Selects the best classifier for each feature, using parallel computing.
    bestClassifiers =
        let parStrategy = evalTuple2 rseq rseq
        in parMap parStrategy bestClassifier features
    
    -- Selects the best classifier threshold for a feature.
    bestClassifier = minimumBy weight . featureClassifiers
    
    -- Lists all possibles classifier configurations associated with theirs
    -- error for a feature and the set of tests.
    featureClassifiers feature =
        -- The first computed classifier will give "True" for each test, so its
        -- error score is the weight of invalid tests.
        fst $ foldl' (\(cs, trueError) (value, weights) ->
            let c1 = (HaarClassifier feature value True, trueError)
                falseError = 1.0 - trueError
                c2 = (HaarClassifier feature value False, falseError)
                
                trueError' = trueError + (sum weights)
            in (c1 : c2 : cs, trueError')
        ) ([], weightInvalid) (featureValues feature tests)

    errorLevel (classifier@(HaarClassifier feature value parity), e) =
        let bad = sum $ map snd $ filter (\(t, w) -> classifier `cClass` t /= tiValid t) tests
        in (e, bad, abs $ e - bad)
    
    -- Sums the weight of all non valid tests.
    !weightInvalid = sum $! map snd $! filter (not . tiValid . fst) tests
    
    weight = compare `on` snd

--     testF feature val =
--         let goods = length $ filter (val >=) $ map fst $ featureValuesSorted feature tests
--             score = sum $ map snd $ filter (\(t, w) -> val < t) $ featureValuesSorted feature tests
--             ns = length $ filter (\(t, w) -> val >= t) $ featureValuesSorted feature tests
--             n = length $ tests
--             valids = length $ filter (\(i, w) -> tiValid i) $ tests
--         in (n, goods, valids, val)

-- | Computes all feature\'s values with a set of tests, sorted and grouped by
-- value.
-- Keeps the test weight. Negative for valid tests, positive for valid tests.
featureValues :: HaarFeature -> [(TrainingImage, Weight)]
                       -> [(Int64, [Weight])]
featureValues feature =
    groupByValue . sortBy (compare `on` fst) . map computeValue
  where
    -- Computes the feature value and its weight.
    computeValue (t, w) =
        let !w' = if tiValid t then w else -w
            !v = compute feature (tiWindow t)
        in (v, w')

    -- Groups the same values in a tuple containing the value and the list of
    -- weights.
    groupByValue =
        map (\((v, w) : xs) -> (v, w : map snd xs)) . groupBy ((==) `on` fst)

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

    putStrLn "Train classifier ..."
    let classifier = adaBoost steps tests selectHaarClassifier
    print classifier

    putStrLn "Save classifier ..."
    writeFile savePath $ show classifier
  where
    loadIntegrals valid = fmap (trainingImages valid) . loadImages

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
trainingImages valid =
    map trainingImage
  where
    rect = Rect 0 0 windowWidth windowHeight
    trainingImage image =
        let ii = II.integralImage image id
            squaredIi = II.integralImage image (^2)
            window = win rect ii squaredIi
        in TrainingImage window valid