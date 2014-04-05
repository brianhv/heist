module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Heist.Interpreted.Tests
import qualified Heist.Compiled.Tests
import qualified Heist.SpliceAPI.Tests
import qualified Heist.Tests

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "Heist.Interpreted.Tests"
                            Heist.Interpreted.Tests.tests
                , testGroup "Heist.Compiled.Tests"
                            Heist.Compiled.Tests.tests
                , testGroup "Heist.SpliceAPI.Tests"
                            Heist.SpliceAPI.Tests.tests
                , testGroup "Heist.Tests"
                            Heist.Tests.tests
                ]
