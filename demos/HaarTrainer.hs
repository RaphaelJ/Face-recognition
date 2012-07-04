import System.Environment (getArgs)

import Vision.Haar.Trainer

main = do
    args <- getArgs
    case args of
        [strSteps, out] -> do
            let steps = read strSteps
            train "../data/learning_faces/" steps out
        _       -> putStrLn "Usage: HaarTrainer <steps> <out file>"