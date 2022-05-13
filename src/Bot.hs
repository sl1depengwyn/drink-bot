module Bot where

import Control.Monad.State
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Database.Database as Database
import qualified GHC.Generics as G
import qualified Logger
import Network.HTTP.Simple (Query)

newtype Host = Tg
  { hUrl :: BC.ByteString
  }
  deriving (Show)

instance A.FromJSON Host where
  parseJSON =
    A.withText "FromJSON Bot.Host" $ \t ->
      case t of
        "tg" -> pure Tg {hUrl = "api.telegram.org"}
        _ -> fail $ "Unknown host: " ++ T.unpack t

data Config = Config
  { cHost :: Host,
    cToken :: T.Text,
    cStartMessage :: T.Text,
    cHelpMessage :: T.Text,
    cFailMessage :: T.Text
  }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle = Handle
  { hConfig :: Config,
    hDatabase :: Database.Handle,
    hLogger :: Logger.Handle
  }

withHandle :: Config -> Database.Handle -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle conf dbHandle lHandle f = f $ Handle {hConfig = conf, hDatabase = dbHandle, hLogger = lHandle}

