import GreyImage
import Primitives
import Data.Array.IArray
import System.Directory

nPix i = fromIntegral $ length $ elems i
sumPix i f = sum $ map (f . fromIntegral) $ elems i
avg i = sumPix i id `div` nPix i
ecart i = sqrt $ fromInteger $ (sumPix i (^2) - ((avg i)^2 * nPix i)) `div` nPix i
norm i = fromInteger . toWord . normalize 127 (avg i) (floor $ ecart i) . fromIntegral
normalize new moy et pix = (pix - moy) * new `div` et + new

main = do
    files <- getDirectoryContents "/home/rapha/Bureau/visages"
    print files
    images <- mapM (\p ->load ("/home/rapha/Bureau/visages/"++p) Nothing) (filter ((/= '.') . head) files)
    let et = map (floor . ecart) images
    print et
    print $ maximum et
    print $ minimum et
    print $ sum et `div` (fromIntegral $ length et) so   