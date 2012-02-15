import System (getArgs)

import Vision.Primitives
import Vision.Images.Image (load, save, resize)

main = do
    path <- head `fmap` getArgs
    i <- load path
    save "out.jpg" $ resize i (Size 764 699) 