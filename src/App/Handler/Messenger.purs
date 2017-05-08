module App.Handler.Messenger where

import App.Types (WebhookEffs, DbRef)
import Control.Monad ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (message, error)
import Control.Monad.Eff.Ref (readRef)
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Messenger.Bot (getMessageEvent, messageEventRunner)
import Messenger.Config (fbConf)
import Messenger.Types (SendEff)
import Messenger.Types.MessageEvent (EventAction(..), Message(..), Response(..), Meta)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getQueryParam, getRouteParam)
import Node.Express.Response (send, setStatus)
import Prelude (bind, otherwise, pure, ($), (<>), (==))
-- import Utils (findByUserId)

-- | TODO logging should depend on dev environment
errorLogger :: forall e. String -> Handler (console :: CONSOLE | e)
errorLogger errorMsg = do
  (liftEff $ log errorMsg) *> (nextThrow $ error errorMsg)


handleMessage :: forall e. Message -> Meta -> SendEff e (Maybe Response)
handleMessage  (Message msg) meta = do
  let text = unNullOrUndefined msg.text
  case text of
    Nothing   -> pure Nothing
    Just txt  ->
      pure $ Just $ Text $ Tuple meta.sender $ "echoed back: "<> txt

messageEventHandler :: forall e. EventAction -> SendEff e (Maybe Response)
messageEventHandler eventAction = do
  case eventAction of
    EventM (Tuple message meta) -> handleMessage message meta
    _              -> pure $ Nothing

verifyFbRequests :: forall e. Handler (console :: CONSOLE | e)
verifyFbRequests = do
  verifyToken <- getQueryParam "verify_token"
  case verifyToken of
    Nothing -> next
    Just token |  token == fbConf.verifyToken -> next
               |  otherwise -> defaultErr token
  where defaultErr token = errorLogger $ "wrong verify token error" <> token


messengerWebhookG :: forall e. Handler (console :: CONSOLE | e)
messengerWebhookG = do
  maybeSubscribe <- getQueryParam "hub.mode"
  case maybeSubscribe of
    Nothing -> defaulErr "No subscribe param"
    Just "subscribe" ->  do
      maybeChallenge <- getQueryParam "hub.challenge"
      case maybeChallenge of
        Nothing -> defaulErr "No challenge"
        Just challenge -> setStatus 200 *> send challenge
    Just any  -> defaulErr any
    where defaulErr msg = errorLogger $ "webhook callback verification error: " <> msg

messengerWebhookP :: forall e. DbRef -> WebhookEffs e
messengerWebhookP dbRef = do
  eitherDb <- liftEff $ readRef dbRef
  case eitherDb of
    Left err -> errorLogger ("DB connection error : " <> message err)
    Right db -> do
      maybeUserId <- getRouteParam "userId"
      case maybeUserId of
        Nothing -> (liftEff $ log "No userId") *> setStatus 200
        Just  userId  -> do
          eitherMessageEvent <- getMessageEvent
          case eitherMessageEvent of
            Left err -> (liftEff $ log $ "message Event error: " <> err) *> setStatus 200
            Right mEvent ->
              (liftEff $ messageEventRunner messageEventHandler mEvent) *> setStatus 200
