module Detector(
      detect, detectImage, loadClassifier
) where

import AdaBoost (Classifier (..), StrongClassifier)
import GreyImage (load, save, drawRectangle)
import HaarClassifier (HaarClassifier)
import IntegralImage (computeIntegralImage)
import Window (wRect, windowsPos)

detect classifierPath imagePath = do
    classifier <- loadClassifier classifierPath
    image <- load imagePath Nothing

    return $ detectImage classifier image

detectImage classifier image =
    let int = computeIntegralImage image id
        squaredInt = computeIntegralImage image (^2)
    in map wRect $ filter (classifier `check`) (windowsPos int squaredInt)

loadClassifier :: String -> IO (StrongClassifier HaarClassifier)
loadClassifier path = fmap read $ readFile path