import Kube.Config (defaultKubeConfigPath)
import System.Environment (setEnv)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testGroup
        "defaultKubeConfigPath"
        [ testCase "from home dir" $ do
            setEnv "HOME" "/home/runner"
            path <- defaultKubeConfigPath
            path @?= "/home/runner/.kube/config"
        , testCase "from kubeconfig" $ do
            setEnv "KUBECONFIG" "/home/runner/.kubeconfig"
            path <- defaultKubeConfigPath
            path @?= "/home/runner/.kubeconfig"
        , testCase "kubeconfig prefer" $ do
            setEnv "HOME" "/home/runner"
            setEnv "KUBECONFIG" "/home/runner/.kubeconfig"
            path <- defaultKubeConfigPath
            path @?= "/home/runner/.kubeconfig"
        ]
    ]
