import Test.Hspec

import qualified UtilsSpec
import qualified ConfigSpec

main :: IO ()
main = hspec spec

spec = do
    describe "Utils" UtilsSpec.spec
    describe "Configuration" ConfigSpec.spec