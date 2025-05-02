{-# LANGUAGE OverloadedStrings #-}

module Kube.ConfigSpec (tests) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TLE
import Kube.Config (decodeBase64, loadKubeConfig)
import Kube.Config.Types.Internal (Config (..), Error (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import qualified Text.Pretty.Simple as PS

decodeBase64Tests :: TestTree
decodeBase64Tests =
  testGroup
    "decodeBase64 Tests"
    [ testCase "Should correctly decode valid Base64 ASCII string" $ do
        let input = "SGVsbG8gV29ybGQh"
        let expected = Right (T.pack "Hello World!")
        let actual = decodeBase64 input
        expected @?= actual,
      testCase "Should return Base64DecodeError for invalid Base64" $ do
        let input = "This is not valid Base64!!!"
        let result = decodeBase64 input
        case result of
          Left (Base64DecodeError _) -> assertBool "" True
          _ -> assertFailure $ "Expected Base64DecodeError, but got: " ++ show result
    ]

configParsingTests :: TestTree
configParsingTests =
  testGroup
    "Config Parsing"
    [ goldenVsString
        "Parsing sample1"
        ("test" </> "data" </> "kubeconfig_sample1.golden")
        (loadAndParseKubeconfig $ "test" </> "data" </> "kubeconfig_sample1.yaml")
    ]
  where
    loadAndParseKubeconfig :: FilePath -> IO BS.ByteString
    loadAndParseKubeconfig yamlPath = do
      yamlContent <- loadKubeConfig yamlPath
      let parsedResult :: Either Error Config
          parsedResult = case yamlContent of
            Left err -> Left $ ParseError (T.pack $ show err)
            Right config -> Right config
      let prettyOutputLazyText :: LT.Text
          prettyOutputLazyText = PS.pShowNoColor parsedResult
      pure $ TLE.encodeUtf8 prettyOutputLazyText

tests :: TestTree
tests =
  testGroup
    "Kube.Config"
    [ decodeBase64Tests,
      configParsingTests
    ]
