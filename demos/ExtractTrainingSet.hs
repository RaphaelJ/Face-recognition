import Control.Monad
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (sha1)
import System.Directory (getDirectoryContents, doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Vision.Haar.Window (nWindows)
import qualified Vision.Image as I
import Vision.Primitive (Size (..))

maxSize :: Int
maxSize = 800

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputDir, outputDir] -> do
            paths <- excludeHidden `fmap` getDirectoryContents inputDir

            sizes <- forM paths $ \path -> do
                let hash = sha1 $ pack path
                let outPath = show hash ++ ".png"

                exists <- doesFileExist $ outputDir </> outPath

                if not exists then do
                    image <- I.load (inputDir </> path) :: IO I.GreyImage
                    let image' = resizeImage image
                    I.save image' (outputDir </> outPath)
                    putStrLn $ path ++ " -> " ++ outPath
                    return $ I.getSize image'
                else do
                    putStrLn $ path ++ " -> " ++ outPath ++ " (exists)"
                    image' <- I.load (outputDir </> outPath) :: IO I.GreyImage
                    return $ I.getSize image'
            
            putStrLn $ show (length paths) ++ " images(s)"
            
            putStrLn $ show (sum $ map nWindows sizes) ++ " window(s)"
        _ -> do
            putStrLn "Prepares images to be used by the HaarTrainer (converts"
            putStrLn "images to grey scale and limits resolution)."
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