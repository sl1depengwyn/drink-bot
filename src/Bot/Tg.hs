module Bot.Tg where

import qualified Bot.Bot                    as Bot
import qualified Bot.Logger                 as Logger
import qualified Bot.Database as Database
import           Control.Monad              (MonadPlus (mzero), replicateM,
                                             void)
import           Control.Monad.State
import qualified Data.Aeson.Extended        as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Yaml                  as Yaml
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

type MapUserToRepeat = Map Int Int

newtype Tg =
  Tg
    { sOffset :: Maybe Int
    }

run :: Bot.Handle -> IO ()
run botH = evalStateT (longPolling botH) (Tg {sOffset = Nothing})

longPolling :: Bot.Handle -> StateT Tg IO ()
longPolling botH =
  forever $ do
    st <- get
    let apiKey = (Bot.cToken . Bot.hConfig) botH
    let offset = sOffset st
    let logH = Bot.hLogger botH
    eitherUpdates <- liftIO $ getUpdates botH offset
    case eitherUpdates of
      (Right updates) -> do
        unless (null updates) (put st {sOffset = Just (uUpdateId (last updates) + 1)})
        mapM_ (processUpdate botH) updates
      (Left err) -> liftIO $ Logger.error logH err

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
      { mFrom :: User
      , mText :: T.Text
      }
  | StickerMessage
      { mFrom    :: User
      , mSticker :: Sticker
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
    , cData :: String 
    }
  deriving (Show, G.Generic)

instance A.FromJSON CallbackQuery where
  parseJSON = A.genericParseJSON A.customOptions

data Update
  = UpdateWithMessage
      { uUpdateId :: Int
      , uMessage  :: Message
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

getUpdates :: Bot.Handle -> Maybe Int -> IO (Either String [Update])
getUpdates botH offset = do
  let host = (Bot.hUrl . Bot.cHost . Bot.hConfig) botH
  let apiKey = (Bot.cToken . Bot.hConfig) botH
  let path = mconcat ["/bot", apiKey, "/getUpdates"]
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  Logger.debug (Bot.hLogger botH) (T.unpack (mconcat [T.decodeUtf8 host, path]))
  response <- httpBS $ buildRequest host (T.encodeUtf8 path) query
  let responseBody = getResponseBody response
  Logger.debug (Bot.hLogger botH) ((T.unpack . T.decodeUtf8) responseBody)
  let updates = A.eitherDecodeStrict responseBody
  pure $ uResult <$> updates

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

newtype InlineKeyboard =
  InlineKeyboard
    { iInlineKeyboard :: [[KeyboardButton]]
    }
  deriving (Show, G.Generic)

instance A.ToJSON InlineKeyboard where
  toJSON = A.genericToJSON A.customOptions

data KeyboardButton =
  SimpleButton
    { bText :: T.Text
    , bCallbackData :: T.Text
    }
  deriving (Show, G.Generic)

instance A.ToJSON KeyboardButton where
  toJSON = A.genericToJSON A.customOptions

replyKeyboard :: InlineKeyboard
replyKeyboard =
  InlineKeyboard
    [[SimpleButton "1" "1", SimpleButton "2" "2"], [SimpleButton "3" "3", SimpleButton "4" "4", SimpleButton "5" "5"]]

type ReceiverId = Int

sendMessage :: Bot.Handle -> ReceiverId -> T.Text -> Query -> StateT Tg IO ()
sendMessage botH usrId method query = do
  let apiKey = (Bot.cToken . Bot.hConfig) botH
  let path = mconcat ["/bot", apiKey, method]
  let senderId = (BC.pack . show) usrId
  let chatId = ("chat_id", Just senderId)
  let finalQuery = chatId : query
  let host = (Bot.hUrl . Bot.cHost . Bot.hConfig) botH
  httpBS (buildRequest host (T.encodeUtf8 path) finalQuery)
  let logH = Bot.hLogger botH
  let logMessage = mconcat ["sent message by ", T.unpack method, " to ", show usrId, " with query ", show finalQuery]
  liftIO $ Logger.info logH logMessage
  

sendKeyboardMessage :: Bot.Handle -> ReceiverId -> StateT Tg IO ()
sendKeyboardMessage botH usrId = sendMessage botH usrId "/sendMessage" query
  where
    repeatText = (Bot.cRepeatMessage . Bot.hConfig) botH
    query = [("text", Just (T.encodeUtf8 repeatText)), ("reply_markup", Just (LC.toStrict (A.encode replyKeyboard)))]


instance Bot.TextBot Tg where
  sendTextMessage botH message usrId = sendMessage botH usrId "/sendMessage" query
    where
      query = [("text", Just (T.encodeUtf8 message))]

sendSticker :: Bot.Handle -> T.Text -> ReceiverId -> StateT Tg IO ()
sendSticker botH fileId usrId = sendMessage botH usrId "/sendSticker" query
  where
    query = [("sticker", Just (T.encodeUtf8 fileId))]

replyMessage :: Bot.Handle -> ReceiverId -> StateT Tg IO () -> StateT Tg IO ()
replyMessage botH usrId sendFunction = do
  let db = Bot.hDatabase botH
  let defNoReps = (Bot.cNumberOfResponses . Bot.hConfig) botH
  noR <- liftIO $ Database.getRepetitions db usrId
  last <$> case noR of
    [] -> replicateM defNoReps sendFunction
    noReps:_       -> replicateM noReps sendFunction

processMessage :: Bot.Handle -> Message -> StateT Tg IO ()
processMessage botH (TextMessage us txt) = do
  let usrId = uId us
  case txt of
    "/help" -> Bot.sendHelpMessage botH usrId
    "/repeat" -> sendKeyboardMessage botH usrId
    txt' -> replyMessage botH usrId (Bot.sendTextMessage botH txt' usrId)
  return ()
processMessage botH (StickerMessage us st) =
  let usrId = uId us
      fileId = sFileId st
   in replyMessage botH usrId (sendSticker botH fileId usrId)
processMessage botH (UnsupportedMessage us) =
  let usrId = uId us
   in Bot.sendFailMessage botH usrId

processCallback :: Bot.Handle -> CallbackQuery -> StateT Tg IO ()
processCallback botH (CallbackQuery (User usrId) reps) = do
  let logH = Bot.hLogger botH
  liftIO $ Database.upsertUser (Bot.hDatabase botH) usrId (read reps)
  let logMsg =
        mconcat ["update in user-repeats db: no of repeats for ", show usrId, " now is ", reps]
  liftIO $ Logger.info logH logMsg

processUpdate :: Bot.Handle -> Update -> StateT Tg IO ()
processUpdate botH (UpdateWithMessage _ msg) = processMessage botH msg
processUpdate botH (UpdateWithCallback _ cb) = processCallback botH cb
