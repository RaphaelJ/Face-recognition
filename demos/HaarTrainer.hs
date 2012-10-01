import System.Environment (getArgs)

import Vision.Haar.Trainer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir, out] -> do
            train dir out
        _     -> do
            putStrLn "Trains a Haar cascade to <out file> using the training"
            putStrLn "images located in <training dir> (which contains a good/"
            putStrLn "and a bad/ directory)."
            putStrLn "Usage: HaarTrainer <training dir> <out file>"