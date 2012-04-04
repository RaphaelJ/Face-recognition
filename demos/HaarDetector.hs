import System.Environment (getArgs)

import Vision.Haar.Detector

main = do
    args <- getArgs
    case args of
        [classifierPath, imagePath] -> do
            rects <- detectImage classifierPath imagePath
            print rects
        _ -> putStrLn "Usage: HaarDetector <classifier> <image>"