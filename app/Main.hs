module Main (main) where

import Config (
  Config (..),
  parseConfig,
 )
import Data.Text.IO qualified as T
import Ledger (ratesToLedger)
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
  value,
 )
import Relude

newtype ConfigFilePath = ConfigFilePath FilePath
  deriving newtype (Show, Eq)

defaultConfigFilePath :: IO (Maybe String)
defaultConfigFilePath = runMaybeT $ do
  configHomeMaybe <- lookupEnv "XDG_CONFIG_HOME"
  configHome <- hoistMaybe configHomeMaybe
  return $ configHome <> "/findata/forex2ledger.toml"

configFilePathP :: Maybe String -> Parser ConfigFilePath
configFilePathP defVal =
  ConfigFilePath
    <$> strOption
      ( long "config_file"
          <> metavar "FILE"
          <> help "The TOML config file."
          <> maybe mempty value defVal
      )

run :: ConfigFilePath -> IO ()
run (ConfigFilePath configFilePath) = do
  eitherConfig <- parseConfig <$> T.readFile configFilePath
  case eitherConfig of
    Left err -> do
      T.hPutStrLn stderr "Could not parse the config file."
      T.hPutStrLn stderr err
      exitFailure
    Right config@(Config _appId base _) -> do
      oerResult <- fetchRates config
      case oerResult of
        Left err -> do
          T.hPutStrLn stderr "Could not fetch rates from OER."
          T.hPutStrLn stderr err
          exitFailure
        Right rates -> do
          T.putStr $ ratesToLedger base rates

main :: IO ()
main = do
  defaultConfigFilePathMaybe <- defaultConfigFilePath
  configFilePath <- execParser (opts defaultConfigFilePathMaybe)
  run configFilePath
 where
  opts defaultConfigFilePathMaybe =
    info
      (configFilePathP defaultConfigFilePathMaybe <**> helper)
      ( fullDesc
          <> progDesc "Print Forex rates in Ledger format."
          <> header "forex2ledger"
      )
