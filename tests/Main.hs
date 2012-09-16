import Test.Framework (defaultMain, testGroup)

import qualified Tests.AI.Learning.AdaBoost as A

import qualified Tests.Data.List.Shuffle as S

import qualified Tests.Vision.Primitive as P
import qualified Tests.Vision.Image as I
import qualified Tests.Vision.Haar as H

main = defaultMain [
      testGroup "AdaBoost" A.tests    
    , testGroup "Shuffle" S.tests    
    , testGroup "Primitives" P.tests
    , testGroup "Images" I.tests
    , testGroup "Haar" H.tests
    ]