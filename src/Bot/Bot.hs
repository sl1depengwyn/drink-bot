module Bot.Bot where

import qualified Bot.Database as Database
import qualified Bot.Logger            as Logger
import qualified Data.Aeson.Extended   as A
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Map              (Map)
import qualified Data.Text             as T
import qualified Data.Yaml             as Yaml
import qualified GHC.Generics          as G
import           Network.HTTP.Simple (Query)
import           Control.Monad.State


newtype Host
  =  Tg
      { hUrl :: BC.ByteString
      }
  deriving (Show)

instance A.FromJSON Host where
  parseJSON =
    A.withText "FromJSON Bot.Host" $ \t ->
      case t of
        "tg" -> pure Tg {hUrl = "api.telegram.org"}
        _    -> fail $ "Unknown host: " ++ T.unpack t

data Config =
  Config
    { cHost              :: Host
    , cToken             :: T.Text
    , cGroupId           :: Maybe T.Text
    , cHelpMessage       :: T.Text
    , cRepeatMessage     :: T.Text
    , cFailMessage       :: T.Text
    , cNumberOfResponses :: Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle =
  Handle
    { hConfig :: Config
    , hDatabase :: Database.Handle
    , hLogger :: Logger.Handle
    }

withHandle :: Config -> Database.Handle -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle conf dbHandle lHandle f = f $ Handle {hConfig = conf, hDatabase = dbHandle, hLogger = lHandle}

type ReceiverId = Int

class TextBot a where
  sendTextMessage :: Handle -> T.Text -> ReceiverId -> StateT a IO ()

sendHelpMessage :: TextBot a => Handle -> ReceiverId -> StateT a IO ()
sendHelpMessage botH = sendTextMessage botH helpText
  where
    helpText = (cHelpMessage . hConfig) botH

sendFailMessage :: TextBot a => Handle -> ReceiverId -> StateT a IO ()
sendFailMessage botH = sendTextMessage botH failText
  where
    failText = (cFailMessage . hConfig) botH