import           Test.Tasty

import qualified Test.HashCores.PipelinedSHA256 as PipeSHA256
import qualified Test.HashCores.SHA256          as SHA256

main = defaultMain $
  testGroup "Hash core tests"
    [ SHA256.tests
    , PipeSHA256.tests
    ]
