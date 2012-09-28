{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Trainer (
    -- * Impure utilities
      train{-, classifierStats-}
    ) where

import Control.Monad
import Data.List
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import AI.Learning.Classifier (StrongClassifier (..), splitTests)

import Vision.Haar.Cascade (
      HaarCascade (..), HaarCascadeStage (..), trainHaarCascade
    , saveHaarCascade, cascadeStats
    )
import Vision.Haar.Window (
      windowWidth, windowHeight, nWindows
    )
import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Size (..))

-- | Trains a strong classifier from directory of tests containing two
-- directories (faces & non_faces).
train :: FilePath -> FilePath -> IO ()
train directory savePath = do
    putStrLn "Loading images ..."

    faces <- loadFaces
    putStrLn $ "\tfaces/ loaded (" ++ show (length faces) ++" images)"

    nonFaces <- loadNonFaces
    let nNonFacesWindows = sum (map (nWindows . II.originalSize . fst) nonFaces)

    putStr $ "\tnon_faces/ loaded (" ++ show (length nonFaces) ++" images, "
    putStrLn $ show nNonFacesWindows ++ " windows)"

    let (facesTraining, facesTesting) = splitTests 0.90 faces
    let nFacesTraining = length facesTraining

    putStrLn $ "Train on " ++ show nFacesTraining ++ " faces ..."
    let cascade = trainHaarCascade faces nonFaces

    -- Prints the stages of the cascade at the same time they are computed.
    forM_ (hcaStages cascade) $ \s -> do
        let nClassifiers = length $ scClassifiers $ hcsClassifier s
        putStrLn $ "New stage: " ++ show nClassifiers ++ " classifiers"
        print s

--     let (detectionRate, falsePositiveRate) = cascadeStats cascade testingSet
-- 
--     putStrLn $ "Detection rate: " ++ show detectionRate
--     putStrLn $ "False positive rate: " ++ show falsePositiveRate

--     classifierStats classifier testingSet

    putStrLn "Save cascade ..."
    saveHaarCascade savePath cascade
  where
    -- Initialises a 'TrainingImage' for each image and its horizontal mirror.
    loadFaces = do
        -- Loads and resizes each image to the detection window\'s size.
        let resize i = I.force $ I.resize i (Size windowWidth windowHeight)
        imgs <- map resize `fmap` loadImages (directory </> "faces")

        -- Computes the horizontal mirror for each valid image.
        let imgs' = (map (I.force . I.horizontalFlip) imgs) ++ imgs

        return $! map integralImages imgs'

    -- Returns an generator of random 'TrainingImage' from the non faces 
    -- images and the number of images and different random windows.
    loadNonFaces = do
        imgs <- map I.force `fmap` loadImages (directory </> "non_faces")
        return $! map integralImages imgs

    integralImages img =
        let ii = II.integralImage img id
            sqii = II.integralImage img (^(2 :: Int))
        in (ii, sqii)

    loadImages dir = do
        files <- (sort . excludeHidden) `fmap` getDirectoryContents dir
        mapM (I.load . (dir </>)) files

    excludeHidden = filter (((/=) '.') . head)

-- -- | Prints the statistics of the sub classifiers of the Haar\'s cascade on a
-- -- set of tests.
-- classifierStats :: HaarCascade -> [TrainingImage] -> IO ()
-- classifierStats classifier tests = do
--     putStrLn $ "Test on " ++ show (length tests) ++ " image(s) ..."
-- 
--     let cs = sortBy (compare `on` snd) $ strongClassifierScores classifier tests
--     putStrLn "Sub classifiers length sorted by score:"
--     forM_ cs $ \(StrongClassifier wcs _, score) -> do
--         putStrLn $ show (length wcs) ++ "\t: " ++ show (score * 100) ++ "%"
-- 
--     let score = classifierScore classifier tests
--     putStrLn $ "Global classifier score is " ++ show (score * 100) ++ "%"