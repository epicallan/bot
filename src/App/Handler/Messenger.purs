module App.Handler.Messenger (
  messengerWebhook
  ) where

import App.Types (WebhookEffs, DbRef)
import Control.Alt (void)
import Control.Monad ((*>))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (readRef)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Messenger.Bot (getMessageEvent, messageEventRunner)
import Messenger.Types (SendEff, Webhook(..))
import Messenger.Types.MessageEvent (EventAction, Response)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (setStatus)
import Prelude (bind, pure, unit, ($), (<>))
import Utils (findByUserId)

messageEventHandler :: forall e. EventAction -> SendEff e (Maybe Response)
messageEventHandler eventAction = do
  liftEff $ log "handler"
  pure $ Nothing


messengerWebhook :: forall e. DbRef -> WebhookEffs e
messengerWebhook dbRef = do
  eitherDb <- liftEff $ readRef dbRef
  case eitherDb of
    Left err ->
      (liftEff $ error $ "DB connection error: " <> message err) *> setStatus 500
    Right db -> do
      maybeUserId <- getRouteParam "userId"
      case maybeUserId of
        Nothing -> (liftEff $ error "No userId") *> setStatus 200
        Just  userId  -> do
          eitherMessageEvent <- getMessageEvent
          case eitherMessageEvent of
            Left err -> (liftEff $ error $ "message Event error" <> err) *> setStatus 200
            Right messageEvent ->
              (liftEff $ runMe userId db messageEvent) *> setStatus 200
  where
    runMe userId db me  = void $ launchAff do
        (maybeWb :: Maybe Webhook) <- findByUserId db "webhooks" userId
        case maybeWb of
          Nothing -> (liftEff $ error $ "No accessToken") *> (pure unit)
          Just (Webhook wb) ->
            (liftEff $ messageEventRunner messageEventHandler wb.accessToken me) *> (pure unit)
