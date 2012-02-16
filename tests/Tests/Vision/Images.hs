{-# LANGUAGE TypeSynonymInstances #-}

module Tests.Vision.Images (
      tests
    ) where

import Control.Applicative
import Test.QuickCheck

import Vision.Primitive (Size (..))
import qualified Vision.Image.RGBAImage as R
import qualified Vision.Image.GreyImage as G

import Tests.Config (maxImageSize)
import qualified Tests.Vision.Primitives

-- instance Arbitrary Image where
--     arbitrary =
--         Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)
        
instance Arbitrary G.GreyImage where
    arbitrary = do
        [size@(Size w h)] <- vector 1 :: Gen [Size]
        G.create size <$> vector (w * h)
        
-- instance Arbitrary IntegralImage where
--     arbitrary =
--         Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)

tests = {-conjoin [-}
              label "Grey => RGBA => Grey" $ propGreyRGBA
--             , label "Grey => Resize 200% => RGBA => Resize 50% => Grey" $
--                 propGreyResize
--         ]

propGreyRGBA :: G.GreyImage -> Bool
propGreyRGBA img = img == G.fromRGBA (G.toRGBA img)

propGreyResize :: G.GreyImage -> Bool
propGreyResize img =
    let Size w h = G.getSize img
        img' = G.fromRGBA $ flip R.resize (Size (w `quot` 2) (h `quot` 2))
             $ G.toRGBA   $ flip G.resize (Size (w * 2)      (h * 2)) $ img
    in img == img'
    
