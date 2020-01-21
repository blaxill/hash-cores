import           Test.Tasty

import qualified Test.HashCores.InPlaceSHA256   as InPlaceSHA256
import qualified Test.HashCores.PipelinedSHA256 as PipeSHA256
import qualified Test.HashCores.SHA256          as SHA256

import System.Environment (setEnv)
import Control.Concurrent (rtsSupportsBoundThreads)

import Prelude

main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "4"
  setEnv "TASTY_SMALLCHECK_DEPTH" "3"
  putStrLn $ "Built with thread support: " ++ show rtsSupportsBoundThreads
  defaultMain $
    testGroup "Hash core tests"
      [ SHA256.tests
      , InPlaceSHA256.tests
      , PipeSHA256.tests
      ]
