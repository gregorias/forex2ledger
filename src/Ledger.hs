{-# LANGUAGE OverloadedStrings #-}

module Ledger (
  ratesToLedger,
) where

import Data.Currency (Alpha)
import Data.Fixed (E6, Fixed)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (
  FormatExtension (ExtendedFormat),
  calendarFormat,
  formatShow,
 )
import Hledger (MarketPrice (MarketPrice))
import OpenExchangeRates (OerRates (..))
import Relude
import Text.Printf (printf)

-- | Renders MarketPrice as Ledger-style text
--
-- @
-- P yyyy-mm-dd from price to
-- @
showMarketPrice :: MarketPrice -> Text
showMarketPrice (MarketPrice day from to price) =
  T.pack $
    printf
      "P %s %s %s %s\n"
      (formatShow (calendarFormat ExtendedFormat) day)
      from
      (show price :: Text)
      to

rateToMarketPrice ::
  Day ->
  -- | the base currency
  Alpha ->
  -- | the quote currency
  Alpha ->
  -- | the price
  Fixed E6 ->
  MarketPrice
rateToMarketPrice day base target price =
  MarketPrice
    day
    (show base)
    (show target)
    (fromRational . toRational $ price)

ratesToMarketPrices ::
  -- | the base currency
  Alpha ->
  OerRates ->
  [MarketPrice]
ratesToMarketPrices base (OerRates day rates) =
  map (uncurry (rateToMarketPrice day base)) $ Map.toList rates

ratesToLedger ::
  -- | the base currency
  Alpha ->
  OerRates ->
  Text
ratesToLedger base =
  T.concat . map showMarketPrice . ratesToMarketPrices base
