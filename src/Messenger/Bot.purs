module Messenger.Bot (
    getMessageEvent
  , messageEventRunner
  ) where
import Control.Bind ((>>=))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (info, warn)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (F)
import Data.Foreign.Generic (decodeJSON)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Messenger.Config (fbConf)
import Messenger.Send (sendResponse)
import Messenger.Types (SendEff)
import Messenger.Types.MessageEvent (EventAction(..), MessageEntry(..), MessageEvent(..), Messaging(..), Response, SenderRecipientId(..))
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getBody)
import Node.Express.Types (EXPRESS)
import Prelude (Unit, bind, join, map, pure, ($))



getEventAction :: { id :: String, time :: Int } -> Messaging -> Maybe EventAction
getEventAction msMeta (Messaging ms) =
  let meta = {  id  : msMeta.id
              , time : msMeta.time
              , timestamp : ms.timestamp
              , sender    : (un SenderRecipientId ms.sender).id
              , recipient : (un SenderRecipientId ms.recipient).id
              }
  in case unNullOrUndefined ms.postback of
    Just postback -> Just (EventP $ Tuple postback meta)
    Nothing -> do
      case unNullOrUndefined ms.message of
        Just message -> Just (EventM $ Tuple message meta)
        Nothing -> do
          case unNullOrUndefined ms.read of
            Just read -> Just (EventR $ Tuple read meta)
            Nothing -> Nothing


messageEventRunner :: forall e. (EventAction -> SendEff e (Maybe Response))
                  -> MessageEvent -> SendEff e Unit
messageEventRunner handler (MessageEvent messageEvent@{ object, entry}) = do
  let eventActions = join $ map (\(MessageEntry messageEntry@{ messaging, id , time })
                          -> map (getEventAction { id, time }) messaging ) entry
      sendResponse' = sendResponse fbConf.accessToken
  liftEff $ traverse_ (\eventAction ->
      case eventAction of
        Nothing    -> liftEff $ info "No event"
        Just event ->
          handler event >>= (maybe (liftEff $ warn "no Response") sendResponse')
          ) eventActions

getMessageEvent :: forall e. HandlerM (express :: EXPRESS | e) (Either String MessageEvent)
getMessageEvent = do
  eitherBodyRaw <- getBody
  case eitherBodyRaw of
    Left multipleBodyErrors -> pure $ Left "error parsing request body"
    Right body -> do
      let eitherMessageEvent = runExcept $ decodeJSON body :: F MessageEvent
      case eitherMessageEvent of
        Left multipleJsonErrors -> pure $ Left "error decoding body to messageEvent"
        Right messageEvent -> pure $ Right messageEvent
