{-# LANGUAGE OverloadedStrings #-}

-- | This module fetches rates from openexchangerates.org
module OpenExchangeRates (
  fetchRates,
  OerRates (..),
) where

import AppId (AppId (AppId))
import Config (Config (Config))
import qualified Data.Aeson as Aeson
import Data.Currency (Alpha, Currency (alpha))
import Data.Fixed (E6, Fixed)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Time (Day, UTCTime (utctDay), secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.HTTP as HTTP
import Network.URL (
  URL (
    URL,
    url_params,
    url_path,
    url_type
  ),
  exportURL,
 )
import qualified Network.URL as URL
import Relude

type CurrencyResolution = E6

data OerRates = OerRates
  { orTimestamp :: !Day
  , orRates :: Map Alpha (Fixed CurrencyResolution)
  }
  deriving stock (Show, Eq)

alphaP :: Text -> Either Text Alpha
alphaP currencyTxt =
  maybeToRight ("Could not parse " `T.append` currencyTxt `T.append` " as currency.\n")
    . readMaybe
    . T.unpack
    $ currencyTxt

currencyInKeysP :: HashMap Text (Fixed CurrencyResolution) -> Either Text (Map Alpha (Fixed CurrencyResolution))
currencyInKeysP =
  convertToMap
    . mapM convertKey
    . HashMap.toList
 where
  convertKey :: (Text, Fixed CurrencyResolution) -> Either Text (Alpha, Fixed CurrencyResolution)
  convertKey = fmap swap . sequence . swap . first alphaP
  convertToMap :: Either Text [(Alpha, Fixed CurrencyResolution)] -> Either Text (Map Alpha (Fixed CurrencyResolution))
  convertToMap = fmap fromList

instance Aeson.FromJSON OerRates where
  parseJSON (Aeson.Object o) =
    OerRates
      <$> timestampP
      <*> ratesP
   where
    timestampP =
      o Aeson..: "timestamp"
        & fmap
          ( utctDay
              . posixSecondsToUTCTime
              . secondsToNominalDiffTime
              . fromInteger
          )
    ratesP = do
      hashMap <- o Aeson..: "rates"
      let eitherRates = currencyInKeysP hashMap
      either (fail . T.unpack) return eitherRates
  parseJSON _ = fail "OpenExchangeRates data should be an object but received something else."

getLatestUrl :: Config -> URL
getLatestUrl (Config (AppId appId) base targets) =
  URL
    { url_type = URL.Absolute (URL.Host (URL.HTTP False) "openexchangerates.org" Nothing)
    , url_path = "/api/latest.json"
    , url_params =
        [ ("app_id", toString appId)
        , ("base", cur2string base)
        , ("symbols", intercalate "," (cur2string <$> targets))
        ]
    }
 where
  cur2string = show . alpha

-- | Fetches rates from openexchangerates.org
fetchRates :: Config -> IO (Either Text OerRates)
fetchRates config = do
  response <- HTTP.simpleHTTP req
  responseCode <- HTTP.getResponseCode response
  body <- HTTP.getResponseBody response
  case responseCode of
    (2, 0, 0) -> do
      return . first T.pack $ Aeson.eitherDecode (fromString body)
    _ ->
      return . Left $
        "Received a non-200 response code:"
          `T.append` T.pack (show responseCode)
          `T.append` " with the response: "
          `T.append` T.pack body
 where
  req = HTTP.getRequest . exportURL . getLatestUrl $ config
