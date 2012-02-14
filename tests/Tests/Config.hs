module Tests.Config (
      maxImageSize
    ) where

import Int

-- | Sets the maximum image width/height to test.
maxImageSize :: Int
maxImageSize = 2000 -- fromIntegral (maxBound :: Int16)