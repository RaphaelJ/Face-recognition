import System (getArgs)

import Vision.Haar.Trainer

main = do
    steps <- fmap (read . head) getArgs
<<<<<<< HEAD
    train "../data/learning_faces/" steps "../data/classifier.cl"
=======
    train "../data/learning_faces_small/" steps "../data/classifier.cl"
>>>>>>> 66d766da7df411b51dfd4eaf9d6df46e4f09d8f1
