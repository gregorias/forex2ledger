{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Config (
  tests,
) where

import Config (
  Config (..),
  parseConfig,
 )
import Data.Currency (Alpha (..))
import Relude
import Test.Hspec (
  SpecWith,
  describe,
  it,
  runIO,
 )
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Config.parseConfig" $ do
    describe "parseConfig" $ do
      sampleConfigText <- runIO (readFileText "config-sample.toml")
      it "parsesTheSampleConfig" $ do
        let config = parseConfig sampleConfigText
        config
          `shouldBe` Right
            ( Config
                "SAMPLEAPPID"
                USD
                [ CHF
                , EUR
                ]
            )
