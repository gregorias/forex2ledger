module Main (main) where

import Relude
import qualified Test.Config as Config
import Test.Hspec (
  SpecWith,
  hspec,
 )
import qualified Test.Ledger as Ledger
import qualified Test.OpenExchangeRates as OpenExchangeRates

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  Config.tests
  Ledger.tests
  OpenExchangeRates.tests
