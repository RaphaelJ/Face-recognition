module Data.List.Shuffle (
    -- * Functions 
      shuffleList, shuffleList'
    ) where

import Control.Monad
import Data.Array (elems)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (runSTArray)
import Data.Function
import Data.List
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import System.Random (mkStdGen, randoms, randomR)

-- | Unsort the elements of a list (complexity: O(n)). Runs with a 'MArray'
-- inside the 'ST' monad. Must be faster of large list.
shuffleList :: [a] -> [a]
shuffleList xs =
    elems $ runSTArray $ do
        arr <- newListArray (0, lastIndex) xs
        genRef <- newSTRef (mkStdGen 1)

        -- Takes a random number between i and the last index of the array
        -- and puts the element at this index at the beginning of the array.
        forM_ [0..lastIndex] $ \i -> do
            gen <- readSTRef genRef
            let (j, gen') = randomR (i, lastIndex) gen
            writeSTRef genRef gen'

            arr `swap` (i, j)

        return arr
  where
    lastIndex = length xs - 1
    arr `swap` (i, j) = do
        iVal <- readArray arr i
        jVal <- readArray arr j
        writeArray arr i jVal
        writeArray arr j iVal

-- | Unsort the elements of a list (complexity: O(n * log² n)). Pure version of
-- 'shuffleList'
shuffleList' :: [a] -> [a]
shuffleList' =
    map snd . sortBy (compare `on` fst) . zip (randoms gen :: [Int])
  where
    gen = mkStdGen 1