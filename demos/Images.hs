import Criterion.Main

import Vision.Primitive
import qualified Vision.Image.RGBAImage as R
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as I

-- path = "image.jpg"
path = "/home/rapha/2012-01-13_14.30.05.206.bmp"
-- path = "/home/rapha/Bureau/test.png"
pathOut = "out.jpg"

main = do
    i <- R.load path
    let grey = G.fromRGBA i
    let ii = I.integralImage grey id
    let Size w h = R.getSize i
--     print $ ii `I.getValue` (Point 300 300)
    let grey_resize = G.resize grey (Size (w `quot` 4) (h `quot` 4))
    putStrLn $ "Image size is " ++ show w ++ " x " ++ show h
    defaultMain [
         bench "load as RGBA" $ whnfIO $ R.load path
{-        , bench "save as RGBA" $ whnfIO $ R.save pathOut i
        , bench "resize 50% RGBA" $
            whnf (R.resize i) (Size (w `quot` 2) (h `quot` 2))
        , bench "resize 200% RGBA" $ whnf (R.resize i) (Size (w * 2) (h * 2))
        -}
        , bench "greyscale from RGBA" $ whnf (G.fromRGBA) i
        {-, bench "greyscale to RGBA" $ whnf (G.toRGBA) grey
        , bench "save greyscale" $ whnfIO $ G.save pathOut grey -}
        , bench "resize 25% greyscale" $
            whnf (G.resize grey) (Size (w `quot` 4) (h `quot` 4)) {-
        , bench "resize 200% greyscale" $
            whnf (G.resize grey) (Size (w * 2) (h * 2))
        -}
        , bench "compute integral image" $
            whnf (I.integralImage grey) id
        , bench "compute integral image 25%" $
            whnf (I.integralImage grey_resize) id
--         , bench "integral image get value" $
--             whnf (ii `I.getValue`) (Point 300 300)
        ]