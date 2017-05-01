module Messenger.Bot where
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, info)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Node.Express.Request (getBody)
import Node.Express.Response (setStatus)
import Messenger.Model.MessageEvent (
  MessageEventHandler, MessageEffs, EventAction(..),
  MessageEntry(..), MessageEvent(..), Messaging(..))
import Data.Foldable (traverse_)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..))
import Prelude (($), bind, (<>))
import Utils (multpleErrorsToStr)

runHandler :: forall e. MessageEntry -> Messaging -> MessageEventHandler e -> MessageEffs e
runHandler mE (Messaging ms) handler = do
  case unNullOrUndefined ms.postback of
    Just postback -> handler mE (EventP postback)
    Nothing -> do
      case unNullOrUndefined ms.message of
        Just message -> handler mE (EventM message)
        Nothing -> do
          case unNullOrUndefined ms.read of
            Just read -> handler mE (EventR read)
            Nothing -> do
              liftEff $ info "unknown Event"
              setStatus 200

messageEventRunner :: forall e. MessageEventHandler e -> MessageEvent -> MessageEffs e
messageEventRunner handler (MessageEvent messageEvent@{ object, entry}) =
  traverse_ (\(MessageEntry messageEntry@{ messaging })
    -> traverseMessaging (MessageEntry messageEntry) messaging) entry
    where traverseMessaging mE = traverse_ (\m -> runHandler mE m handler)

webhookMessagesHandler :: forall e. MessageEventHandler e -> MessageEffs e
webhookMessagesHandler handler = do
  eitherBodyRaw <- getBody
  case eitherBodyRaw of
    Left multipleBodyErrors -> do
      liftEff $ error $ multpleErrorsToStr multipleBodyErrors
      setStatus(200)
    Right body -> do
      let eitherMessageEvent = runExcept $ readJSON body :: F MessageEvent
      case eitherMessageEvent of
        Left multipleJsonErrors -> do
          liftEff $ error $ "failed to purse json: " <> (multpleErrorsToStr multipleJsonErrors)
          setStatus(200)
        Right messageEvent -> messageEventRunner handler messageEvent
