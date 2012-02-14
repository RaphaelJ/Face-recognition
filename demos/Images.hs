import System (getArgs)

import Vision.Images.GreyImage (load, save)

main = do
    path <- (read . head) `fmap` getArgs
    load path >>= save "out.png"