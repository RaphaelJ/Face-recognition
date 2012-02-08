import System (getArgs)

import Vision.Haar.Trainer

main = do
    steps <- fmap (read . head) getArgs
    train "LearningTests" steps "classifier.cl"