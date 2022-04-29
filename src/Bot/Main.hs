module Bot.Main where

import qualified Bot.Bot             as Bot
import qualified Bot.Logger          as Logger
import qualified Bot.Database as Database
import qualified Bot.Tg              as Tg
import qualified Data.Aeson.Extended as A
import qualified Data.Yaml           as Yaml

import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure)
import qualified GHC.Generics as G

data Config =
  Config
    { cDatabase :: Database.Config 
    ,  cBot    :: Bot.Config
    , cLogger :: Logger.Config
    }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [configPath] -> run configPath
    _            -> run "config.yaml"

run :: FilePath -> IO ()
run path = do
  errOrConfig <- Yaml.decodeFileEither path
  conf <- either (fail . show) pure errOrConfig
  let db = cDatabase conf
  let bot = cBot conf
  let logger = cLogger conf
  let toRun =
        case Bot.cHost bot of
          Bot.Tg _ -> Tg.run
  Logger.withHandle logger (\hLogger -> Database.withHandle db (\hDb ->  Bot.withHandle bot hDb hLogger toRun))
