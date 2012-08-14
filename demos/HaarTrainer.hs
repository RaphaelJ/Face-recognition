import System.Environment (getArgs)

import Vision.Haar.Trainer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [out] -> do
            train "../data/training_faces/" out
        _     -> do
            putStrLn "Train a Haar cascade to <out file> using the training"
            putStrLn "images located in ../data/training_faces/."
            putStrLn "Usage: HaarTrainer <out file>"