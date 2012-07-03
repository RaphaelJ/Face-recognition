module Vision.Haar.Detector (
    -- * Functions
      detect
    -- * Impure utilities
    , loadClassifier
    ) where

import Data.Function
import Data.List
import System.FilePath (FilePath)

import AI.Learning.Classifier (Classifier (..), StrongClassifier, Weight)
import Vision.Haar.Classifier (HaarClassifier)
import Vision.Haar.Window (wRect, windows)
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Rect)

-- | Detects all positive matchs inside the image using a strong
-- 'HaarClassifier'.
detect :: StrongClassifier HaarClassifier -> G.GreyImage -> [(Rect, Weight)]
detect classifier image =
    let integral = II.integralImage image id
        squaredIintegral = II.integralImage image (^2)
        wins = windows integral squaredIintegral
        rects = map (\w -> (wRect w, classifier `cClassScore` w)) wins
        valids = filter (\(r, (v, s)) -> v) rects
    in reverse $ map (\(r, (v, s)) -> (r, s)) $ sortBy (compare `on` (snd . snd)) valids

-- | Loads a strong 'HaarClassifier'.
loadClassifier :: FilePath -> IO (StrongClassifier HaarClassifier)
loadClassifier path = read `fmap` readFile path