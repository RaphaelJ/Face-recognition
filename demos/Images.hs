import System (getArgs)

import Vision.Primitive
import Vision.Image.RGBAImage (load, save, resize)

main = do
    path <- head `fmap` getArgs
    i <- load path
    save "out.jpg" i -- $ resize i (Size 764 699) 