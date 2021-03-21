module Spec (main) where

import Relude
import qualified Test.Config as Config
import Test.Hspec (
  SpecWith,
  hspec,
 )
import qualified Test.OpenExchangeRates as OpenExchangeRates

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  Config.tests
  OpenExchangeRates.tests
