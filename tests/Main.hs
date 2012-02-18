import Test.QuickCheck

import qualified Tests.Vision.Primitive as P
import qualified Tests.Vision.Image as I

main = quickCheck I.tests