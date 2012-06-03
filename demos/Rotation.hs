import Data.Array
import Data.List
import Debug.Trace
import System.Environment

import qualified Image as I
import Primitives

main = do
    filename <- (!! 0) `fmap` getArgs
    angle <- (read . head) `fmap` getArgs
    image <- I.load filename Nothing
    I.save "out.png" (rotate angle image)

rotate angle image =
    accumArray takeNew black (bounds image) $ foldl' acc [] (assocs image)
  where
    Size w h = I.getSize image
    (centerX, centerY) = (fromIntegral w / 2.0, fromIntegral h / 2.0)
    acc xs (c, p) | Just c' <- coords' c = (c', p) : xs
                  | otherwise            = xs
    black = I.Pixel 0 0 0
    takeNew _ p = p
    coords' (Point x y) = 
        let (x', y') = (fromIntegral x - centerX, fromIntegral y - centerY)
            x'' = round $ x' * cos angle - y' * sin angle + centerX
            y'' = round $ x' * sin angle + y' * cos angle + centerY
        in if x'' >= 0 && x'' < w && y'' >= 0 && y'' < h
              then Just $ Point x'' y''
              else Nothing