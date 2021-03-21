{-# LANGUAGE OverloadedStrings #-}

module Test.OpenExchangeRates (
  tests,
) where

import qualified Data.Aeson as Aeson
import Data.Currency (Alpha (..))
import qualified Data.Map.Strict as Map
import Data.Time (fromGregorian)
import OpenExchangeRates (OerRates (OerRates))
import Relude
import Test.Hspec (
  SpecWith,
  describe,
  it,
 )
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = describe "OpenExchangeRates" $ do
  describe "OerRates" $ do
    describe "Aeson.FromJSON" $ do
      it "parsesTheSampleConfig" $ do
        let json =
              "{\n\
              \   \"disclaimer\": \"Usage subject to terms: https://openexchangerates.org/terms\",\n\
              \   \"license\": \"https://openexchangerates.org/license\",\n\
              \   \"timestamp\": 1616007600,\n\
              \   \"base\": \"USD\",\n\
              \   \"rates\": {\n\
              \     \"CHF\": 0.922272\n\
              \   }\n\
              \ }"
        Aeson.eitherDecode json
          `shouldBe` Right
            ( OerRates
                (fromGregorian 2021 3 17)
                (Map.singleton CHF 0.922272)
            )
