{-# LANGUAGE OverloadedStrings #-}

-- | This module parses the config file.
module Config (
  Config (..),
  parseConfig,
) where

import AppId (AppId (..))
import Data.Currency (Alpha)
import Relude
import Toml (TomlBiMap, TomlCodec, prettyTomlDecodeErrors, (.=))
import qualified Toml

data Config = Config
  { configOpenExchangeRateAppId :: !AppId
  , configBaseCurrency :: !Alpha
  , configTargetCurrencies :: [Alpha]
  }
  deriving stock (Show, Eq)

alphaBiMap :: TomlBiMap Data.Currency.Alpha Toml.AnyValue
alphaBiMap = Toml._EnumBounded

currencyBiMap :: TomlBiMap Alpha Toml.AnyValue
currencyBiMap = alphaBiMap

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.diwrap (Toml.text "oer_app_id") .= configOpenExchangeRateAppId
    <*> Toml.match currencyBiMap "currency.base" .= configBaseCurrency
    <*> Toml.arrayOf currencyBiMap "currency.targets" .= configTargetCurrencies

parseConfig :: Text -> Either Text Config
parseConfig = first prettyTomlDecodeErrors . Toml.decode configCodec
