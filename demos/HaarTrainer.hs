import System.Environment (getArgs)

import Vision.Haar.Trainer

main = do
    args <- getArgs
    case args of
        [strSteps] -> do
            let steps = read strSteps
            train "../data/learning_faces/" steps "../data/classifier.cl"
        _       -> putStrLn "Usage: HaarTrainer <steps>"