import System (getArgs)

import Vision.Haar.Trainer

main = do
    steps <- fmap (read . head) getArgs
    train "../data/learning_faces_small/" steps "../data/classifier.cl"