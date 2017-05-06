module App.Handler.Messenger (
  messengerWebhook
  ) where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Messenger.Bot (getMessageEvent, messageEventRunner)
import Messenger.Model.MessageEvent (EventAction, MessageEffs, Response, WebHookEffs)
import Messenger.Types (SendEff)
import Node.Express.Response (setStatus)
import Prelude (pure, ($), bind)

messageEventHandler :: forall e. EventAction -> SendEff e (Maybe Response)
messageEventHandler eventAction = do
  liftEff $ log "handler"
  pure $ Nothing

messengerWebhook :: forall e. WebHookEffs e
messengerWebhook = do
  -- get userId param to respond to particular user
  liftEff $ log $ "message webhook"
  eitherMessageEvent <- getMessageEvent
  case eitherMessageEvent of
    Left err -> do
      liftEff $ error $ err
      setStatus 200
    Right messageEvent -> do
      liftEff $ messageEventRunner messageEventHandler messageEvent
      setStatus 200
