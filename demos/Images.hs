import System (getArgs)

import Criterion.Main

import Vision.Primitive
import qualified Vision.Image.RGBAImage as R
import qualified Vision.Image.GreyImage as G

path = "/home/rapha/Bureau/image.jpg"
pathOut = "out.jpg"

main = do
    i <- R.load path
    let Size w h = R.getSize i
    putStrLn $ "Image size is " ++ show w ++ " x " ++ show h
    let grey = G.fromRGBA i
    defaultMain [
          bench "load as RGBA" $ whnfIO $ R.load path
        , bench "save as RGBA" $ whnfIO $ R.save pathOut i
        , bench "resize 50% RGBA" $
            whnf (R.resize i) (Size (w `quot` 2) (h `quot` 2))
        , bench "resize 200% RGBA" $ whnf (R.resize i) (Size (w * 2) (h * 2))
        
        , bench "greyscale from RGBA" $ whnf (G.fromRGBA) i
        , bench "greyscale to RGBA" $ whnf (G.toRGBA) grey
        , bench "resize 50% greyscale" $
            whnf (G.resize grey) (Size (w `quot` 2) (h `quot` 2))
        , bench "resize 200% greyscale" $
            whnf (G.resize grey) (Size (w * 2) (h * 2))
        ]