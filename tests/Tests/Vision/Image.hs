{-# LANGUAGE TypeSynonymInstances #-}

module Tests.Vision.Image (
      tests
    ) where

import Control.Applicative
import Test.QuickCheck

import Debug.Trace

import Vision.Primitive (Size (..))
import qualified Vision.Image.RGBAImage as R
import qualified Vision.Image.GreyImage as G

import Tests.Config (maxImageSize)
import qualified Tests.Vision.Primitive

instance Arbitrary R.RGBAImage where
    arbitrary = do
        [size@(Size w h)] <- vector 1 :: Gen [Size]
        R.create size <$> vector (w * h * 4)
        
instance Arbitrary G.GreyImage where
    arbitrary = do
        [size@(Size w h)] <- vector 1 :: Gen [Size]
        G.create size <$> vector (w * h)
        
-- instance Arbitrary IntegralImage where
--     arbitrary =
--         Size <$> choose (0, maxImageSize) <*> choose (0, maxImageSize)

tests = conjoin [
              label "Grey to/from RGBA" $ propGreyRGBA
            , label "RGBA resize" $ propRGBAResize
            , label "Grey resize" $ propGreyResize
        ]

propGreyRGBA :: G.GreyImage -> Bool
propGreyRGBA img = img == G.fromRGBA (G.toRGBA img)

propRGBAResize :: R.RGBAImage -> Bool
propRGBAResize img =
    let size@(Size w h) = R.getSize img
    in img == R.resize (R.resize img (Size (w * 2) (h * 2))) size

propGreyResize :: G.GreyImage -> Bool
propGreyResize img =
    let size@(Size w h) = G.getSize img
    in img == G.resize (G.resize img (Size (w * 2) (h * 2))) size