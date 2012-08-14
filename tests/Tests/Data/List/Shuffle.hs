module Tests.Data.List.Shuffle (
      tests
    ) where
        
import Data.List
import Data.List.Shuffle (shuffleList, shuffleList')

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests = [
      testProperty "sort a shuffled List" propShuffleSort
    , testProperty "sort a purely shuffled List" propPureShuffleSort
    ]

propShuffleSort :: [Int] -> Bool
propShuffleSort xs =
    sort xs == sort (shuffleList xs)

propPureShuffleSort :: [Int] -> Bool
propPureShuffleSort xs =
    sort xs == sort (shuffleList' xs)