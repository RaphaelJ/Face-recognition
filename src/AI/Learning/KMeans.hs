-- | Gives an implementation of a generic k-means clustering algorithm.
-- See <http://en.wikipedia.org/wiki/K-means_clustering> for implementation\'s
-- details.
module Learning.KMeans (
      KMeansFlag (..)
    , kmeans, weightedKmeans
    ) where

data KMeansFlag = RandomCenters | KMeansPP

       
weightedKmeans :: Int -- ^ Number of clusters.
               -> Int -- ^ Number of iterations.
               -- | List of points of N dimensions with an associated weight.
               -> [[(Double, Int)]]
               -> 