module Tg where

import qualified Bot
import Control.Monad
  ( MonadPlus (mzero),
    replicateM,
    void,
  )
import Control.Monad.State
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Map (Map)
import qualified Data.Map as Map
--import qualified Data.Yaml as Yaml

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.Database as Database
import qualified GHC.Generics as G
import qualified Logger
import Network.HTTP.Simple

newtype Tg = Tg
  { sOffset :: Maybe Int
  }

run :: Bot.Handle -> IO ()
run h = evalStateT (longPolling h) (Tg {sOffset = Nothing})

longPolling :: Bot.Handle -> StateT Tg IO ()
longPolling h =
  forever $ do
    st <- get
    let offset = sOffset st
    let logh = Bot.hLogger h
    eitherUpdates <- liftIO $ getUpdates h offset
    case eitherUpdates of
      (Right updates) -> do
        unless (null updates) (put st {sOffset = Just (uUpdateId (last updates) + 1)})
        mapM_ (processUpdate h) updates
      (Left err) -> do
        liftIO $ Logger.error logh err
        put st {sOffset = (+ 1) <$> offset}

newtype User = User
  { uId :: Int
  }
  deriving (Show, G.Generic)

instance A.FromJSON User where
  parseJSON = A.genericParseJSON A.customOptions

newtype Sticker = Sticker
  { sFileId :: T.Text
  }
  deriving (Show, G.Generic)

instance A.FromJSON Sticker where
  parseJSON = A.genericParseJSON A.customOptions

data Message
  = TextMessage
      { mFrom :: User,
        mMessageId :: Int,
        mText :: T.Text,
        mDate :: Int -- stored as unix time
      }
  | UnsupportedMessage
      { mFrom :: User
      }
  deriving (Show, G.Generic)

instance A.FromJSON Message where
  parseJSON = A.genericParseJSON A.customOptions

data CallbackQuery = CallbackQuery
  { cFrom :: User,
    cData :: String
  }
  deriving (Show, G.Generic)

instance A.FromJSON CallbackQuery where
  parseJSON = A.genericParseJSON A.customOptions

data Update
  = UpdateWithMessage
      { uUpdateId :: Int,
        uMessage :: Message
      }
  | UpdateWithEdit
      { uUpdateId :: Int,
        uEditedMessage :: Message
      }
  | UpdateWithCallback
      { uUpdateId :: Int,
        uCallbackQuery :: CallbackQuery
      }
  deriving (Show, G.Generic)

instance A.FromJSON Update where
  parseJSON = A.genericParseJSON A.customOptions

newtype UpdatesResponse = UpdatesResponse
  { uResult :: [Update]
  }
  deriving (Show, G.Generic)

instance A.FromJSON UpdatesResponse where
  parseJSON = A.genericParseJSON A.customOptions

newtype InlineKeyboard = InlineKeyboard
  { iInlineKeyboard :: [[KeyboardButton]]
  }
  deriving (Show, G.Generic)

instance A.ToJSON InlineKeyboard where
  toJSON = A.genericToJSON A.customOptions

data KeyboardButton = SimpleButton
  { bText :: T.Text,
    bCallbackData :: T.Text
  }
  deriving (Show, G.Generic)

instance A.ToJSON KeyboardButton where
  toJSON = A.genericToJSON A.customOptions

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
    setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

sendRequest :: MonadIO m => Bot.Handle -> T.Text -> Query -> m (Response BC.ByteString)
sendRequest h method query = do
  let host = (Bot.hUrl . Bot.cHost . Bot.hConfig) h
  let apiKey = (Bot.cToken . Bot.hConfig) h
  let path = mconcat ["/bot", apiKey, method]
  let logh = Bot.hLogger h
  httpBS $ buildRequest host (T.encodeUtf8 path) query

getUpdates :: MonadIO m => Bot.Handle -> Maybe Int -> m (Either String [Update])
getUpdates h offset = do
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  response <- sendRequest h "/getUpdates" query
  let responseBody = getResponseBody response
  let updates = A.eitherDecodeStrict responseBody
  pure $ uResult <$> updates

sendMessage :: MonadIO m => Bot.Handle -> Int -> T.Text -> Query -> m ()
sendMessage h usrId method query = do
  let chatId = ("chat_id", Just $ (BC.pack . show) usrId)
  let finalQuery = chatId : query
  response <- sendRequest h method finalQuery
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  let respParser obj = case A.parseMaybe (\o -> (o A..: "result") >>= (A..: "message_id")) obj of
        (Just mId) -> liftIO $ Database.updateLastMessageId dbh usrId mId
        Nothing -> liftIO $ Logger.error logh "No message id found in response to send message"
  either (liftIO . Logger.error logh) respParser (A.eitherDecodeStrict (getResponseBody response))
  let logMessage = mconcat ["sent message by ", T.unpack method, " to ", show usrId, " with query ", show finalQuery]
  liftIO $ Logger.info logh logMessage

sendTextMessage :: MonadIO m => Bot.Handle -> T.Text -> Int -> m ()
sendTextMessage botH message usrId = sendMessage botH usrId "/sendMessage" query
  where
    query = [("text", Just (T.encodeUtf8 message))]

sendSticker :: MonadIO m => Bot.Handle -> T.Text -> Int -> m ()
sendSticker botH fileId usrId = sendMessage botH usrId "/sendSticker" query
  where
    query = [("sticker", Just (T.encodeUtf8 fileId))]

sendStartMsg :: MonadIO m => Bot.Handle -> Int -> m ()
sendStartMsg h = sendTextMessage h (Bot.cStartMessage . Bot.hConfig $ h)

sendHelpMsg :: MonadIO m => Bot.Handle -> Int -> m ()
sendHelpMsg h = sendTextMessage h (Bot.cHelpMessage . Bot.hConfig $ h)

editMessage :: MonadIO m => Bot.Handle -> Int -> Int -> T.Text -> m ()
editMessage h usrId mId txt = do
  let query =
        [ ("message_id", Just $ (BC.pack . show) mId),
          ("chat_id", Just $ (BC.pack . show) usrId),
          ("text", Just $ T.encodeUtf8 txt)
        ]
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  sendRequest h "/editMessageText" query
  let logMessage = mconcat ["edited message with id ", show mId, " to ", show txt, " with query ", show query]
  liftIO $ Logger.info logh logMessage

processMessage :: MonadIO m => Bot.Handle -> Message -> m ()
processMessage h (UnsupportedMessage user) = undefined
processMessage h msg@(TextMessage user mId txt _) = case T.words txt of
  "/start" : _ -> sendStartMsg >> addUserToDb h usrId
  "/help" : _ -> sendHelpMsg
  "/add" : val -> undefined
  "/remove" : val -> undefined
  _ -> addRecordToDb h msg
  where
    usrId = uId user

processEdit :: Bot.Handle -> Message -> IO ()
processEdit h (UnsupportedMessage user) = undefined
processEdit h (TextMessage user mId txt _) = do
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  let usrId = uId user
  case reads (T.unpack txt) of
    [] -> pure ()
    (amount, _) : _ -> do
      Database.updateRecord dbh usrId mId amount
      sumOfaDay <- Database.getTodaysAmount dbh usrId
      mIdToEdit <- Database.getLastMessageId dbh usrId
      case (sumOfaDay, mIdToEdit) of
        (Just (Just s), Just mId) -> do
          let msgText = mconcat ["Today you have drinked ", T.pack $ show s]
          editMessage h usrId mId msgText
        _ -> do
          let logText = mconcat ["error in processEdit function, this is (sumOfaDay, mIdToEdit):", show (sumOfaDay, mIdToEdit)]
          Logger.error logh logText

processCallback :: Bot.Handle -> CallbackQuery -> StateT Tg IO ()
processCallback botH (CallbackQuery (User usrId) reps) = undefined

processUpdate :: Bot.Handle -> Update -> StateT Tg IO ()
processUpdate botH (UpdateWithMessage _ msg) = processMessage botH msg
processUpdate botH (UpdateWithEdit _ msg) = liftIO $ processEdit botH msg
processUpdate botH (UpdateWithCallback _ cb) = processCallback botH cb

addRecordToDb :: MonadIO m => Bot.Handle -> Message -> m ()
addRecordToDb h msg = do
  let txt = mText msg
  let usrId = uId $ mFrom msg
  let mId = mMessageId msg
  let t = posixSecondsToUTCTime $ realToFrac $ mDate msg
  let logh = Bot.hLogger h
  case reads (T.unpack txt) of
    [] -> pure ()
    (amount, _) : _ -> do
      let dbh = Bot.hDatabase h
      liftIO $ Database.addRecord dbh usrId mId amount t
      sumOfaDay <- liftIO $ Database.getTodaysAmount dbh usrId
      case sumOfaDay of
        (Just (Just s)) -> do
          let msgText = mconcat ["Today you have drinked ", T.pack $ show s]
          sendTextMessage h msgText usrId
        _ -> do
          let logText = mconcat ["error in addRecordTodb function, this is sumOfaDay: ", show sumOfaDay]
          liftIO $ Logger.error logh logText

addUserToDb :: MonadIO m => Bot.Handle -> Int -> m ()
addUserToDb h usrId = liftIO $ Database.addUser (Bot.hDatabase h) usrId
