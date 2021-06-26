import Test.Hspec

import qualified UtilsSpec

main :: IO ()
main = hspec spec

spec = do
    describe "Utils" UtilsSpec.spec
   