import Control.Monad
import Data.List
import System.Environment (getArgs)
import System.FilePath (splitFileName, (</>))

import Vision.Haar.Detector
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G

main = do
    args <- getArgs
    case args of
        [classifierPath, imagePath, outPath] -> do
            c <- loadClassifier classifierPath
            detectFaces c imagePath outPath
        [classifierPath] -> do
            c <- loadClassifier classifierPath
            imagesPaths <- lines `fmap` getContents
            forM_ imagesPaths $ \imagePath -> do
                let (dir, file) = splitFileName imagePath
                detectFaces c imagePath (dir </> "out" </> file)
        _ ->
            putStrLn "Usage: HaarDetector <classifier> [<image> <output>]"

detectFaces classifier imagePath outPath = do
    i <- I.load imagePath :: IO G.GreyImage
    let rs = detect classifier i
    I.save outPath $ drawRectangles i (map fst rs)
    putStrLn outPath
    print rs

drawRectangles i rs =
    foldl' step i rs
  where
    step acc r = I.drawRectangle acc id (const 255) r