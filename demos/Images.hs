import Criterion.Main

import qualified Vision.Image as I
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive

path = "visages.jpg"
pathOut = "out.png"

main = do
    i <- I.load path :: IO I.RGBAImage
    grey <- I.load path
    let ii = II.integralImage grey id
    let Size w h = I.getSize grey
    let grey_resize = I.resize grey (Size (w `quot` 4) (h `quot` 4))
    putStrLn $ "Image size is " ++ show w ++ " x " ++ show h
    putStrLn ""
    
    defaultMain [
          bench "load as RGBA" $ whnfIO $ (I.load path :: IO I.RGBAImage)
--         , bench "save as RGBA" $ whnfIO $ I.save pathOut i
        , bench "resize 50% RGBA" $
            whnf (I.resize i) (Size (w `quot` 2) (h `quot` 2))
        , bench "resize 200% RGBA" $
            whnf (I.resize i) (Size (w * 2) (h * 2))

        , bench "greyscale to RGBA" $
            whnf (I.convert :: I.GreyImage -> I.RGBAImage) grey
--         , bench "save greyscale" $  whnfIO $ I.save pathOut grey
        
        , bench "resize 25% greyscale" $
            whnf (I.resize grey) (Size (w `quot` 4) (h `quot` 4))
        , bench "resize 200% greyscale" $
            whnf (I.resize grey) (Size (w * 2) (h * 2))
           
        , bench "compute integral image" $ 
            whnf (II.integralImage grey) id
        , bench "compute integral image squared" $ 
            whnf (II.integralImage grey) (^2)
        , bench "compute integral image 25%" $
            whnf (II.integralImage grey_resize) id
        , bench "integral image get value" $
            whnf (ii `I.getPixel`) (Point (w `quot` 2) (h `quot` 2))
        ]