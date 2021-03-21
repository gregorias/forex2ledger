{-# LANGUAGE OverloadedStrings #-}

-- | This module parses the config file.
module Config (
  Config (..),
  parseConfig,
) where

import AppId (AppId (..))
import Data.Currency (Currency (alpha), fromAlpha)
import qualified Data.Currency
import Relude
import Toml (TomlBiMap, TomlCodec, prettyTomlDecodeErrors, (.=))
import qualified Toml

data Config = Config
  { configOpenExchangeRateAppId :: !AppId
  , configBaseCurrency :: !Currency
  , configTargetCurrencies :: [Currency]
  }
  deriving stock (Show, Eq)

currencyAlphaBiMap :: TomlBiMap Currency Data.Currency.Alpha
currencyAlphaBiMap = Toml.iso alpha fromAlpha

alphaBiMap :: TomlBiMap Data.Currency.Alpha Toml.AnyValue
alphaBiMap = Toml._EnumBounded

currencyBiMap :: TomlBiMap Currency Toml.AnyValue
currencyBiMap = currencyAlphaBiMap >>> alphaBiMap

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.diwrap (Toml.text "oer_app_id") .= configOpenExchangeRateAppId
    <*> Toml.match currencyBiMap "currency.base" .= configBaseCurrency
    <*> Toml.arrayOf currencyBiMap "currency.targets" .= configTargetCurrencies

parseConfig :: Text -> Either Text Config
parseConfig = first prettyTomlDecodeErrors . Toml.decode configCodec
