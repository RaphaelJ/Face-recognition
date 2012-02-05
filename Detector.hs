module Detector(
    -- * Functions
      detect
    -- * Impure utilities
    , detectImage, loadClassifier
    ) where

import System.FilePath (FilePath)

import AdaBoost (Classifier (..), StrongClassifier)
import GreyImage (load, save, drawRectangle)
import HaarClassifier (HaarClassifier)
import IntegralImage (computeIntegralImage)
import Window (wRect, windows)

-- | Detects all positive matchs inside the image using a strong
-- 'HaarClassifier'.
detect :: StrongClassifier HaarClassifier -> GreyImage -> IO [Rect]
detect classifier image =
    let int = computeIntegralImage image id
        squaredInt = computeIntegralImage image (^2)
    in map wRect $ filter (classifier `check`) (windows int squaredInt)

-- | Loads a strong 'HaarClassifier' and an image and detects all positive
-- matchs.
detectImage :: FilePath -> FilePath -> IO [Rect]
detectImage classifierPath imagePath = do
    classifier <- loadClassifier classifierPath
    image <- load imagePath Nothing

    return $ detectImage classifier image

-- | Loads a strong 'HaarClassifier'.
loadClassifier :: FilePath -> IO (StrongClassifier HaarClassifier)
loadClassifier path = read `fmap` readFile path