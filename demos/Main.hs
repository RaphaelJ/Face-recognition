import System (getArgs)

import Trainer

main = do
    steps <- fmap (read . head) getArgs
    train "LearningTests" steps "classifier.cl"