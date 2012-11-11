-- | Gives an implementation of a generic k-means clustering algorithm.
-- See <http://en.wikipedia.org/wiki/K-means_clustering> for implementation\'s
-- details.
module AI.Learning.KMeans (
      Point, KMeansFlag (..)
    , kmeans, weightedKmeans, dist
    ) where

import Data.List
import System.Random (RandomGen)

import Data.List.Shuffle (shuffleList)

-- | A point in a N dimensions space.
type Point = [Double]

data KMeanInit = RandomCenters | KMeansPP | Initial [Point]

weightedKmeans :: Int -- ^ Number of clusters.
               -> Int -- ^ Number of iterations.
               -- | List of points of N dimensions with an associated weight.
               -> [(Point, Double)]
               -> KMeanInit -- ^ The way the first clusters will be initialised.
               -> [Point]
weightedKmeans k iters ps method =
    
  where
    -- Computes the centers from randomly selected groups of points.
    randomCenters = map center $ split (shuffleList ps) (len `quot` k)
    -- 

    len = length ps

-- | Computes the squared euclidean distance between two points.
-- See <http://en.wikipedia.org/wiki/Euclidean_distance>.
dist :: Point -> Point -> Double
dist a b = sum [ d * d | i <- a | j <- b, let d = i - j ]

-- | Computes the center of a cluster of weighted point by averaging the 
-- coordinates.
center :: [(Point, Double)] -> Point
center ps =
    -- Takes the weighted average for each dimensions.
    map (\(xs, w) -> sum xs / sumWeights) $ transpose weightedPs
  where
    -- Each point\'s dimensions multiplied/normalized by their weights.
    weightedPs = [ (map (* w) xs, w) | (xs, w) <- ps ]
    sumWeights = sum $ map snd ps

-- | Selects a random item from the list, using a associated weight for each
-- element of the list.
weightedRandom :: RandomGen g => [(a, Double)] -> g -> (g, a)
weightedRandom ps gen =
    (gen', findRandom 0 ps)
  where
    (gen', rand) = randomR (0, sumWeights) gen
    sumWeights = sum $ map snd $ ps

    -- Finds the first point for which the cumulative weights of the list is
    -- greater than the randomly selected number.
    findRandom accWeights  ((p, _):[])  = p
    findRandom accWeights ~((p, w):ps') =
        let accWeights' = accWeights + w
        in if accWeights' > rand
              then p
              else findRandom accWeights' ps'

-- | Unsort the elements of a list (complexity: O(n * log² n)) which are 
-- associated with a weight.
weightedShuffle :: 

shuffleList' :: RandomGen g => [a] -> g
shuffleList' =
    map snd . sortBy (compare `on` fst) . zip (randoms gen :: [Int])
  where
    gen = mkStdGen 1


-- | Divides the list in sublists of the given length (except the last sublist
-- which will contain the remainder of the original list).
split :: [a] -> Int -> [[a]]
split [] _ = []
split xs n =
    let (ys, zs) = splitAt n xs
    in ys : split zs n

double :: Integral a => a -> Double
double = fromIntegral