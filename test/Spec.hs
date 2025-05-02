import Kube.ConfigSpec qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "kube-hs Tests"
    [ Kube.ConfigSpec.tests
    ]
