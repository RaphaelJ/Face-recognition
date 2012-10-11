import Control.Monad
import Data.List
import System.Environment (getArgs)
import System.FilePath (splitFileName, (</>))

import Vision.Haar.Cascade (HaarCascade, loadHaarCascade)
import Vision.Haar.Detector (detect)
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G
import Vision.Primitive (Rect)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cascadePath, imagePath, outPath] -> do
            c <- loadHaarCascade cascadePath
            detectFaces c imagePath outPath
        [cascadePath] -> do
            putStrLn "Read files names from stdin ..."
            c <- loadHaarCascade cascadePath
            imagesPaths <- lines `fmap` getContents

            forM_ imagesPaths $ \imagePath -> do
                let (dir, file) = splitFileName imagePath
                detectFaces c imagePath (dir </> "out" </> file)
        _ -> do
            putStrLn "Detect faces in an image using an Haar cascade."
            putStrLn "Without <image> and <ouput>, detect faces from paths"
            putStrLn "given to stdin."
            putStrLn "Usage: HaarDetector <classifier> [<image> <output>]"

detectFaces :: HaarCascade -> FilePath -> FilePath -> IO ()
detectFaces cascade imagePath outPath = do
    i <- I.load imagePath :: IO G.GreyImage
    let rs = detect cascade i
    print $ length rs
    I.save (drawRectangles i (map fst rs)) outPath
    putStrLn outPath
    print rs

drawRectangles :: (Num p, I.Image i p a) => i -> [Rect] -> i
drawRectangles i rs =
    foldl' step i rs
  where
    step acc r = I.drawRectangle acc id (const 255) r