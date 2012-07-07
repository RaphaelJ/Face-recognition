module Vision.Haar.Detector (
    -- * Functions
      detect
    -- * Impure utilities
    , loadClassifier
    ) where

import Control.Parallel.Strategies
import Data.Function
import Data.List
import System.FilePath (FilePath)

import AI.Learning.Classifier (Classifier (..), StrongClassifier (..), Weight)
import Vision.Haar.Classifier (HaarClassifier)
import Vision.Haar.Window (wRect, windows, Win (..))
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Rect (..))

-- | Detects all positive matchs inside the image using a strong
-- 'HaarClassifier'.
detect :: StrongClassifier HaarClassifier -> G.GreyImage -> [(Rect, Weight)]
detect classifier image =
    let integral = II.integralImage image id
        squaredIintegral = II.integralImage image (^2)
        wins = windows integral squaredIintegral
        rects = map (\w -> (wRect w, classifier `cClassScore` w)) wins
        valids = map (\(w, (v, s)) -> (w, s)) $ filter (\(w, (v, s)) -> v) rects
--         valids = [ (r, s) | w <- windows integral squaredIintegral
--             , let r = wRect w, let (v, s) = classifier `cClassScore` w, v
--             ]
    in reverse $ sortBy (compare `on` snd) valids

-- | Loads a strong 'HaarClassifier'.
loadClassifier :: FilePath -> IO (StrongClassifier HaarClassifier)
loadClassifier path = read `fmap` readFile path