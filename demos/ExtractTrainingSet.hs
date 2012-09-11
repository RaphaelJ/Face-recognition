import Control.Monad
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Vision.Image as I
import Vision.Primitive (Size (..))

maxSize :: Int
maxSize = 640

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputDir, outputDir] -> do
            paths <- excludeHidden `fmap` getDirectoryContents inputDir
            
            forM_ (zip [1..] paths) $ \(i, path) -> do
                image <- I.load (inputDir </> path) :: IO I.GreyImage
                let outPath = show (i :: Int) ++ ".png"
                
                putStrLn $ path ++ " -> " ++ outPath
                
                I.save (resizeImage image) (outputDir </> outPath)
        _ -> do
            putStrLn "Prepares images to be used by the HaarTrainer (converts"
            putStrLn "images to grey scale and limit reselution)."
            putStrLn "Usage: ExtractTrainingSet <input dir> <output dir>"
  where
    excludeHidden = filter (((/=) '.') . head)

    resizeImage image =
        let Size w h = I.getSize image
            largest = max w h
            w' = w * maxSize `quot` largest
            h' = h * maxSize `quot` largest
        in if w > maxSize || h > maxSize
            then I.resize image (Size w' h')
            else image