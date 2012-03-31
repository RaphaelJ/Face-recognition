import Test.Framework (defaultMain, testGroup)

import qualified Tests.Vision.Primitive as P
import qualified Tests.Vision.Image as I

main = defaultMain [
      testGroup "Primitives" P.tests
    , testGroup "Images" I.tests
    ]