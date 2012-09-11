{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Vision.Haar.Trainer (
    -- * Impure utilities
      train{-, classifierStats-}
    ) where

import Data.List
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Random (mkStdGen, next)


import AI.Learning.Classifier (
      splitTests, classifierScore, strongClassifierScores
    )

import Vision.Haar.Cascade (
      HaarCascade (..), trainHaarCascade, cascadeStats
    )
import Vision.Haar.Classifier (TrainingImage (..)) 
import Vision.Haar.Window (
      win, windowWidth, windowHeight, randomWindows, nWindows
    )
import qualified Vision.Image as I
import qualified Vision.Image.GreyImage as G
import qualified Vision.Image.IntegralImage as II
import Vision.Primitive (Size (..), Rect (..))

-- | Trains a strong classifier from directory of tests containing two
-- directories (faces & non_faces).
train :: FilePath -> FilePath -> IO ()
train directory savePath = do
    putStrLn "Loading images ..."
    
    faces <- loadFaces
    putStrLn $ "\tfaces/ loaded (" ++ show (length faces) ++" images)"
    
    (winGen, nNonFaces, nNonFacesWindows) <- loadNonFaces
    putStr $ "\tnon_faces/ loaded (" ++ show nNonFaces ++" images, "
    putStrLn $ show nNonFacesWindows ++ " windows)"
    
    let (facesTraining, facesTesting) = splitTests 0.90 faces
    let nFacesTraining = length facesTraining

    putStrLn $ "Train on " ++ show nFacesTraining ++ " faces ..."
--     let !cascade = trainHaarCascade facesTraining winGen
--     let !cascade = trainHaarCascade facesTraining (mkStdGen 1) winGen
    let !cascade = trainHaarCascade facesTraining (winGen $ mkStdGen 1)
    print cascade
    
--     let (detectionRate, falsePositiveRate) = cascadeStats cascade testingSet
--     
--     putStrLn $ "Detection rate: " ++ show detectionRate
--     putStrLn $ "False positive rate: " ++ show falsePositiveRate

--     classifierStats classifier testingSet
    
    putStrLn "Save cascade ..."
    writeFile savePath $ show cascade
  where
    -- | Initialises a 'TrainingImage' for each image and its horizontal mirror.
    loadFaces = do
        -- Loads and resizes each image to the detection window\'s size.
        let resize i = I.force $ I.resize i (Size windowWidth windowHeight)
        imgs <- map resize `fmap` loadImages (directory </> "faces")
        
        -- Computes the horizontal mirror for each valid image.
        let imgs' = (map (I.force . I.horizontalFlip) imgs) ++ imgs
        
        return [ TrainingImage w True | img <- imgs'
            , let (ii, sqii) = integralImages img
            , let w = win (Rect 0 0 windowWidth windowHeight) ii sqii
            ]
    
    -- | Returns an generator of random 'TrainingImage' from the non faces 
    -- images and the number of images and different random windows.
    loadNonFaces = do
        imgs <- map I.force `fmap` loadImages (directory </> "non_faces")
        let iimgs = map integralImages imgs
        let winGen gen = [ TrainingImage w False 
                | w <- randomImagesWindows iimgs gen 
                ]
        let nNonFaces = length imgs
        let nNonFacesWindows = sum (map (nWindows . I.getSize) imgs)
        return (winGen, nNonFaces, nNonFacesWindows)
        
    -- | Given a list of imgs, returns an infinite random list of windows.
    -- The first window comes from the first image, the second window from 
    -- the second image and so on.
    randomImagesWindows iimgs gen = 
        go imgsWindows []
      where
        -- Consumes the list of infinite lists of windows by taking a window
        -- from each list at a time.
        -- > [ [a1, a2, a3 ..], [b1, b2, b3 ..], [c1, c2, c3 ..] ]
        -- becomes:
        -- > [ a1, b1, c1, a2, b2, c2, a3, b3, c3 .. ]
        go []           acc =
            go (reverse acc) []
        go ~((x:xs):ys) acc =
            x : go ys (xs:acc)
        
        -- Returns the list of the infinite random lists of windows for each 
        -- image.
        imgsWindows = [ randomWindows (mkStdGen $ randomVal * i) ii sqii
            | (i, (ii, sqii)) <- zip [1..] iimgs
            ]
        
        randomVal = fst $ next gen
   
    integralImages img =
        let ii = II.integralImage img id
            sqii = II.integralImage img (^(2 :: Int))
        in (ii, sqii)

    loadImages dir = do
        files <- (sort . excludeHidden) `fmap` getDirectoryContents dir
        mapM (I.load . (dir </>)) files

    excludeHidden = filter (((/=) '.') . head)

-- -- | Prints the statistics of the sub classifiers of the Haar\'s cascade on a
-- -- set of tests.
-- classifierStats :: HaarCascade -> [TrainingImage] -> IO ()
-- classifierStats classifier tests = do
--     putStrLn $ "Test on " ++ show (length tests) ++ " image(s) ..."
--     
--     let cs = sortBy (compare `on` snd) $ strongClassifierScores classifier tests
--     putStrLn "Sub classifiers length sorted by score:"
--     forM_ cs $ \(StrongClassifier wcs _, score) -> do
--         putStrLn $ show (length wcs) ++ "\t: " ++ show (score * 100) ++ "%"
--         
--     let score = classifierScore classifier tests
--     putStrLn $ "Global classifier score is " ++ show (score * 100) ++ "%"