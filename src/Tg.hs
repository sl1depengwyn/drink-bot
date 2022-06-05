module Tg where

import qualified Bot
import           Control.Monad                         (MonadPlus (mzero),
                                                        replicateM, void)
import           Control.Monad.State
import qualified Data.Aeson.Extended                   as A
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Lazy.Char8            as LC
import           Data.Map                              (Map)
import qualified Data.Map                              as Map

--import qualified Data.Yaml as Yaml
import           Data.Maybe                            (fromJust)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           Data.Text.Lazy                        as TL (unpack)
import           Data.Text.Lazy.Encoding               as TL (decodeUtf8)
import           Data.Time
import           Data.Time.Clock.POSIX                 (posixSecondsToUTCTime)
import qualified Database.Database                     as Database
import qualified GHC.Generics                          as G
import qualified Logger
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit           (responseTimeoutNone)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple                   hiding (httpLbs)
import qualified Plotter.Plotter                       as Plotter
import           System.Directory.Extended             (removeIfExists)

newtype Tg =
  Tg
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

newtype User =
  User
    { uId :: Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON User where
  parseJSON = A.genericParseJSON A.customOptions

newtype Sticker =
  Sticker
    { sFileId :: T.Text
    }
  deriving (Show, G.Generic)

instance A.FromJSON Sticker where
  parseJSON = A.genericParseJSON A.customOptions

data Message
  = TextMessage
      { mFrom      :: User
      , mMessageId :: Int
      , mText      :: T.Text
      , mDate      :: Int -- stored as unix time
      }
  | UnsupportedMessage
      { mFrom :: User
      }
  deriving (Show, G.Generic)

instance A.FromJSON Message where
  parseJSON = A.genericParseJSON A.customOptions

data CallbackQuery =
  CallbackQuery
    { cFrom :: User
    , cId   :: T.Text
    , cData :: T.Text
    }
  deriving (Show, G.Generic)

instance A.FromJSON CallbackQuery where
  parseJSON = A.genericParseJSON A.customOptions

data Update
  = UpdateWithMessage
      { uUpdateId :: Int
      , uMessage  :: Message
      }
  | UpdateWithEdit
      { uUpdateId      :: Int
      , uEditedMessage :: Message
      }
  | UpdateWithCallback
      { uUpdateId      :: Int
      , uCallbackQuery :: CallbackQuery
      }
  deriving (Show, G.Generic)

instance A.FromJSON Update where
  parseJSON = A.genericParseJSON A.customOptions

newtype UpdatesResponse =
  UpdatesResponse
    { uResult :: [Update]
    }
  deriving (Show, G.Generic)

instance A.FromJSON UpdatesResponse where
  parseJSON = A.genericParseJSON A.customOptions

newtype InlineKeyboard =
  InlineKeyboard
    { iInlineKeyboard :: [[KeyboardButton]]
    }
  deriving (Show, G.Generic)

instance A.ToJSON InlineKeyboard where
  toJSON = A.genericToJSON A.customOptions

data KeyboardButton =
  SimpleButton
    { bText         :: T.Text
    , bCallbackData :: T.Text
    }
  deriving (Show, G.Generic)

instance A.ToJSON KeyboardButton where
  toJSON = A.genericToJSON A.customOptions

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $
  setRequestPath path $
  setRequestSecure True $ setRequestPort 443 $ setRequestResponseTimeout responseTimeoutNone defaultRequest

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
  kb' <- liftIO $ getKeyboard h usrId
  let finalQuery = chatId : query
  response <- sendRequest h method finalQuery
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  let respParser obj =
        case A.parseMaybe (\o -> (o A..: "result") >>= (A..: "message_id")) obj of
          (Just mId) -> liftIO $ Database.updateLastMessageId dbh usrId mId
          Nothing -> liftIO $ Logger.error logh "No message id found in response to send message"
  either (liftIO . Logger.error logh) respParser (A.eitherDecodeStrict (getResponseBody response))
  let logMessage = mconcat ["sent message by ", T.unpack method, " to ", show usrId, " with query ", show finalQuery]
  liftIO $ Logger.info logh logMessage

sendPhotoWithSumAndKb :: MonadIO m => Bot.Handle -> Int -> m ()
sendPhotoWithSumAndKb h usrId = do
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  msgText' <- getTextWithAmount h usrId
  case msgText' of
    (Just msgText) -> do
      kb <- liftIO $ getKeyboard h usrId
      let fileId = Bot.cPicture . Bot.hConfig $ h
      let query =
            [ ("photo", Just (T.encodeUtf8 fileId))
            , ("caption", Just (T.encodeUtf8 msgText))
            , ("reply_markup", Just $ A.encodeStrict kb)
            ]
      sendMessage h usrId "/sendPhoto" query
    _ -> pure () -- error handled in getTextWithAmount

sendTextMessage :: MonadIO m => Bot.Handle -> T.Text -> Int -> m ()
sendTextMessage botH message usrId = sendMessage botH usrId "/sendMessage" query
  where
    query = [("text", Just (T.encodeUtf8 message))]

sendSticker :: MonadIO m => Bot.Handle -> T.Text -> Int -> m ()
sendSticker botH fileId usrId = sendMessage botH usrId "/sendSticker" query
  where
    query = [("sticker", Just (T.encodeUtf8 fileId))]

sendStartMsg :: MonadIO m => Bot.Handle -> Int -> m ()
sendStartMsg h usrId = do
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  let msgText = Bot.cStartMessage . Bot.hConfig $ h
  kb <- liftIO $ getKeyboard h usrId
  let fileId = Bot.cPicture . Bot.hConfig $ h
  let query =
        [ ("photo", Just (T.encodeUtf8 fileId))
        , ("caption", Just (T.encodeUtf8 msgText))
        , ("reply_markup", Just $ A.encodeStrict kb)
        ]
  sendMessage h usrId "/sendPhoto" query

sendHelpMsg :: MonadIO m => Bot.Handle -> Int -> m ()
sendHelpMsg h = sendTextMessage h (Bot.cHelpMessage . Bot.hConfig $ h)

sendFailMsg :: MonadIO m => Bot.Handle -> Int -> m ()
sendFailMsg h = sendTextMessage h (Bot.cFailMessage . Bot.hConfig $ h)

editMessage :: MonadIO m => Bot.Handle -> Int -> Int -> Query -> m ()
editMessage h usrId mId query' = do
  let query = [("message_id", Just $ (BC.pack . show) mId), ("chat_id", Just $ (BC.pack . show) usrId)] ++ query'
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  resp <- sendRequest h "/editMessageCaption" query
  liftIO $ Logger.debug logh (BC.unpack $ getResponseBody resp)
  let logMessage = mconcat ["edited message with id ", show mId, " with query ", show query]
  liftIO $ Logger.info logh logMessage

editLastMessage :: Bot.Handle -> Int -> IO ()
editLastMessage h usrId = do
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  msgText' <- getTextWithAmount h usrId
  mIdToEdit <- Database.getLastMessageId dbh usrId
  case (msgText', mIdToEdit) of
    (Just msgText, Just mId) -> do
      kb <- liftIO $ getKeyboard h usrId
      let rmarkup = ("reply_markup", Just $ A.encodeStrict kb)
      editMessage h usrId mId [("caption", Just $ T.encodeUtf8 msgText), rmarkup]
    _ -> do
      let logText =
            mconcat ["error in editLastMessage function, this is (msgText', mIdToEdit):", show (msgText', mIdToEdit)]
      Logger.error logh logText

editLastMessagePhoto :: Bot.Handle -> Int -> FilePath -> IO ()
editLastMessagePhoto h usrId pathToPhoto = do
  let host = Bot.hUrl . Bot.cHost . Bot.hConfig $ h
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  let apiKey = (Bot.cToken . Bot.hConfig) h
  let path = mconcat ["/bot", apiKey, "/editMessageMedia"]
  kb <- liftIO $ getKeyboard h usrId
  mIdToEdit <- Database.getLastMessageId dbh usrId
  msgText' <- getTextWithAmount h usrId
  case (mIdToEdit, msgText') of
    (Just mId, Just msgText) -> do
      kb <- getKeyboard h usrId
      let req =
            buildRequest
              host
              (T.encodeUtf8 path)
              [ ("chat_id", Just (BC.pack . show $ usrId))
              , ("message_id", Just (BC.pack . show $ mId))
              , ("media", Just "{\"type\": \"photo\", \"media\": \"attach://photo\"}")
              , ("caption", Just $ T.encodeUtf8 msgText)
              , ("reply_markup", Just $ A.encodeStrict kb)
              ]
      m <- newManager tlsManagerSettings
      req' <- formDataBody [partFileSource "photo" pathToPhoto] req
      resp <- httpLbs req' m
      Logger.debug logh (TL.unpack . TL.decodeUtf8 $ getResponseBody resp)
    _ -> do
      let logText = mconcat ["error in editLastMessage function, this is mIdToEdit:", show mIdToEdit]
      Logger.error logh logText

processMessage :: MonadIO m => Bot.Handle -> Message -> m ()
processMessage h (UnsupportedMessage user) = do
  let logh = Bot.hLogger h
  let logmsg = mconcat ["got unsupported message from ", show user]
  liftIO $ Logger.warning logh logmsg
  sendFailMsg h (uId user)
processMessage h (TextMessage user mId txt t') =
  case T.words txt of
    "/start":_ -> sendStartMsg h usrId >> addUserToDb h usrId
    "/help":_ -> sendHelpMsg h usrId
    "/add":val -> liftIO $ addButton h usrId val >> sendPhotoWithSumAndKb h usrId
    "/remove":val -> liftIO $ removeButton h usrId val >> sendPhotoWithSumAndKb h usrId
    _ -> addRecordToDb h usrId mId txt t >> sendPhotoWithSumAndKb h usrId
  where
    usrId = uId user
    t = posixSecondsToUTCTime $ realToFrac t'

processEdit :: Bot.Handle -> Message -> IO ()
processEdit h (UnsupportedMessage user) = do
  let logh = Bot.hLogger h
  let logmsg = mconcat ["edit to unsupported message ", show user]
  liftIO $ Logger.warning logh logmsg
processEdit h m@(TextMessage user mId txt _) = do
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  let usrId = uId user
  case reads (T.unpack txt) of
    [] -> liftIO $ Logger.warning logh $ mconcat ["cant parse edit ", show m]
    (amount, _):_ -> do
      Database.updateRecord dbh usrId mId amount
      editLastMessage h usrId

processCallback :: MonadIO m => Bot.Handle -> CallbackQuery -> m ()
processCallback h (CallbackQuery (User usrId) cid' "todayStats") = do
  let dbh = Bot.hDatabase h
  let filename = T.unpack (cid' <> ".png")
  let ph = Bot.hPlotter h
  stats <- liftIO $ Database.getTodaysRecordsStats dbh usrId
  liftIO $ Plotter.plotStats ph stats filename
  liftIO $ editLastMessagePhoto h usrId filename
  liftIO $ removeIfExists filename
processCallback h (CallbackQuery (User usrId) cid' "thisMonthStats") = do
  let dbh = Bot.hDatabase h
  let filename = T.unpack (cid' <> ".png")
  let ph = Bot.hPlotter h
  stats <- liftIO $ Database.getMonthRecordsStats dbh usrId
  liftIO $ Plotter.plotStats ph stats filename
  liftIO $ editLastMessagePhoto h usrId filename
  liftIO $ removeIfExists filename
processCallback h (CallbackQuery (User usrId) cid' amount) = do
  t <- liftIO getCurrentTime
  let cid = read $ T.unpack cid'
  addRecordToDb h usrId cid amount t
  answerCallback h (T.encodeUtf8 cid') "Keep going!"
  liftIO $ editLastMessage h usrId

answerCallback :: MonadIO m => Bot.Handle -> BC.ByteString -> BC.ByteString -> m (Response BC.ByteString)
answerCallback h cid text = sendRequest h "/answerCallbackQuery" query
  where
    query = [("callback_query_id", Just cid), ("text", Just text)]

processUpdate :: Bot.Handle -> Update -> StateT Tg IO ()
processUpdate h (UpdateWithMessage _ msg) = processMessage h msg
processUpdate h (UpdateWithEdit _ msg)    = liftIO $ processEdit h msg
processUpdate h (UpdateWithCallback _ cb) = processCallback h cb

addRecordToDb :: MonadIO f => Bot.Handle -> Int -> Int -> T.Text -> UTCTime -> f ()
addRecordToDb h usrId mId txt t = do
  let logh = Bot.hLogger h
  let logmsg = mconcat ["cant parse new record from ", show usrId, ", msg text: ", T.unpack txt]
  case reads (T.unpack txt) of
    [] -> liftIO $ Logger.warning logh $ mconcat []
    (amount, _):_ -> do
      let dbh = Bot.hDatabase h
      liftIO $ Database.addRecord dbh usrId mId amount t

addUserToDb :: MonadIO m => Bot.Handle -> Int -> m ()
addUserToDb h usrId = do
  let initkb = Bot.cInitKeyboard . Bot.hConfig $ h
  let dbh = Bot.hDatabase h
  liftIO $ Database.addUser dbh usrId
  liftIO $ mapM_ (Database.addButton dbh usrId) initkb

addButton :: Bot.Handle -> Int -> [T.Text] -> IO ()
addButton h usrId [] = Logger.error (Bot.hLogger h) "no parameter to /add command"
addButton h usrId (amount':_) = do
  case reads (T.unpack amount') of
    [] -> Logger.error (Bot.hLogger h) $ "bad parameter to /add command " ++ T.unpack amount'
    (amount, _):_ -> do
      let dbh = Bot.hDatabase h
      let logh = Bot.hLogger h
      let logText = mconcat ["added button ", show amount, " for user ", show usrId]
      Logger.info logh logText
      Database.addButton dbh usrId amount

removeButton :: Bot.Handle -> Int -> [T.Text] -> IO ()
removeButton h usrId [] = Logger.error (Bot.hLogger h) "no parameter to /remove command"
removeButton h usrId (amount':_) = do
  case reads (T.unpack amount') of
    [] -> Logger.error (Bot.hLogger h) $ "bad parameter to /add command" ++ T.unpack amount'
    (amount, _):_ -> do
      let dbh = Bot.hDatabase h
      let logh = Bot.hLogger h
      let logText = mconcat ["added button ", show amount, " for user ", show usrId]
      Logger.info logh logText
      Database.removeButton dbh usrId amount

getKeyboard :: Bot.Handle -> Int -> IO InlineKeyboard
getKeyboard h usrId = do
  let dbh = Bot.hDatabase h
  kb <- Database.getButtons dbh usrId
  pure $
    InlineKeyboard
      [ [SimpleButton "Stats for today 📊" "todayStats", SimpleButton "Stats for this month 📊" "thisMonthStats"]
      , map
          (\x ->
             let x' = T.pack . show $ x
              in SimpleButton x' x')
          kb
      ]

getTextWithAmount :: MonadIO m => Bot.Handle -> Int -> m (Maybe T.Text)
getTextWithAmount h usrId = do
  let dbh = Bot.hDatabase h
  let logh = Bot.hLogger h
  sumOfaDay <- liftIO $ Database.getSumTodaysAmount dbh usrId
  case sumOfaDay of
    (Just (Just s)) -> do
      pure $ Just $ mconcat ["Today you have drinked ", T.pack $ show s]
    _ -> do
      let logText = mconcat ["error in getTextWithAmount function, this is sumOfaDay: ", show sumOfaDay]
      liftIO $ Logger.error logh logText
      pure Nothing
