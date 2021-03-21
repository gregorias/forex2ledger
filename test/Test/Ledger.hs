{-# LANGUAGE OverloadedStrings #-}

module Test.Ledger (
  tests,
) where

import Data.Currency (Alpha (..))
import qualified Data.Map.Strict as Map
import Data.Time (fromGregorian)
import Ledger (ratesToLedger)
import OpenExchangeRates (OerRates (OerRates))
import Relude
import Test.Hspec (
  SpecWith,
  describe,
  it,
 )
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = describe "Ledger" $ do
  describe "ratesToLedger" $ do
    it "formatsRates" $ do
      ratesToLedger
        USD
        ( OerRates
            (fromGregorian 2021 3 17)
            ( Map.singleton CHF 0.922272
                <> Map.singleton EUR 1.1
            )
        )
        `shouldBe` "P 2021-03-17 USD 0.922272 CHF\n\
                   \P 2021-03-17 USD 1.1 EUR\n"
