{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Trainer (
    -- * Impure utilities
      train
    ) where

import Control.Monad
import Data.List
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import AI.Learning.Classifier (StrongClassifier (..), splitTests)

import Vision.Haar.Cascade (
      HaarCascade (..), HaarCascadeStage (..), maxFalsePositive
    , trainHaarCascade, saveHaarCascade, cascadeStats
    )
import Vision.Haar.Window (
      windows, windowWidth, windowHeight, nWindows, randomImagesWindows
    )
import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Size (..))

import System.Random (mkStdGen)

-- | Trains a 'HaarCascade' from directory of tests containing two directories
-- (good/ & bad/) and save it.
train :: FilePath -> FilePath -> IO ()
train directory savePath = do
    putStrLn "Loading images ..."

    good <- loadGood
    putStrLn $ "\tgood/ loaded (" ++ show (length good) ++" images)"

    bad <- loadBad
    let nBadWindows = sum (map (nWindows . II.originalSize . fst) bad)

    putStr $ "\tbad/ loaded (" ++ show (length bad) ++" images, "
    putStrLn $ show nBadWindows ++ " windows)"

    let (goodTraining, goodTesting) = splitTests 0.90 good
    let (badTraining, badTesting) = splitTests 0.90 bad

    let goodTestingWindows = concatMap (uncurry windows) goodTesting
    let randomBadTestingWindows = randomImagesWindows (mkStdGen 1) badTesting

    let nBadTestingWindows = round $ 1 / maxFalsePositive
    let badTestingWindows = take nBadTestingWindows randomBadTestingWindows

    putStrLn $ "Train on " ++ show (length goodTraining) ++ " valid images ..."
    let cascade = trainHaarCascade goodTraining badTraining

    let stages = hcaStages cascade

    -- Prints the stages of the cascade at the same time they are computed
    -- and test the current cascade on the testing set.
    forM_ (zip [1..] stages) $ \(i, s) -> do
        let nFeatures = length $ scClassifiers $ hcsClassifier s
        putStrLn $ "New stage: " ++ show nFeatures ++ " features"
        print s

        let curCascade = HaarCascade (take i stages)
        let score = cascadeStats curCascade goodTestingWindows badTestingWindows
        putStrLn "Current cascade score: "
        putStrLn $ "Detection rate: " ++ show (fst score)
        putStrLn $ "False positive rate: " ++ show (snd score)

    putStrLn "Save cascade ..."
    saveHaarCascade savePath cascade
  where
    -- Computes the integral images for each image and its horizontal mirror
    -- from the good/ directory.
    loadGood = do
        -- Loads and resizes each image to the detection window\'s size.
        let resize i = I.force $ I.resize i (Size windowWidth windowHeight)
        imgs <- map resize `fmap` loadImages (directory </> "good")

        -- Computes the horizontal mirror for each valid image.
        let imgs' = (map (I.force . I.horizontalFlip) imgs) ++ imgs

        return $! map integralImages imgs'

    -- Computes the integral images for invalid images from the bad/ directory.
    loadBad = do
        imgs <- loadImages (directory </> "bad")
        return $! map integralImages imgs

    integralImages img =
        let ii = II.integralImage img id
            sqii = II.integralImage img (^(2 :: Int))
        in (ii, sqii)

    loadImages dir = do
        files <- (sort . excludeHidden) `fmap` getDirectoryContents dir
        mapM (I.load . (dir </>)) files

    excludeHidden = filter (((/=) '.') . head)