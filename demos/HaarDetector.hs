import Data.List
import System.Environment (getArgs)

import Vision.Haar.Detector
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G

main = do
    args <- getArgs
    case args of
        [classifierPath, imagePath, outPath] -> do
            i <- I.load imagePath :: IO G.GreyImage
            c <- loadClassifier classifierPath
            let rs = detect c i
            I.save outPath $ drawRectangles i (take 15 $ map fst rs)
            
            print rs
        _ ->
            putStrLn "Usage: HaarDetector <classifier> <image> <output>"

drawRectangles i rs =
    foldl' step i rs
  where
    step acc r = I.drawRectangle acc id (const 255) r