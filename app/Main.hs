{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config (
  Config (..),
  parseConfig,
 )
import qualified Data.Text.IO as T
import OpenExchangeRates (fetchRates)
import Options.Applicative (
  Parser,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  progDesc,
  strOption,
 )
import Relude

newtype ConfigFilePath = ConfigFilePath FilePath
  deriving newtype (Show, Eq)

configFilePathP :: Parser ConfigFilePath
configFilePathP =
  ConfigFilePath
    <$> strOption
      ( long "config_file"
          <> metavar "FILE"
          <> help "The TOML config file."
      )

run :: ConfigFilePath -> IO ()
run (ConfigFilePath configFilePath) = do
  eitherConfig <- parseConfig <$> T.readFile configFilePath
  case eitherConfig of
    Left err -> do
      T.hPutStrLn stderr "Could not parse the config file."
      T.hPutStrLn stderr err
      exitFailure
    Right config@(Config _appId _ _) -> do
      tmpText <- fetchRates config
      case tmpText of
        Left err -> do
          T.hPutStrLn stderr "Could not fetch rates from OER."
          T.hPutStrLn stderr err
          exitFailure
        Right result -> do
          T.putStrLn $ show result

main :: IO ()
main = do
  configFilePath <- execParser opts
  run configFilePath
 where
  opts =
    info
      (configFilePathP <**> helper)
      ( fullDesc
          <> progDesc "Print Forex rates in Ledger format."
          <> header "forex2ledger"
      )
