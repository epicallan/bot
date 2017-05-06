module Messenger.Bot (
    getMessageEvent
  , messageEventRunner
  ) where
import Control.Bind ((>>=))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (info, log, warn)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Messenger.Model.MessageEvent (EventAction(..), MessageEffs, MessageEntry(..), MessageEvent(..), Messaging(..), Response(..))
import Messenger.Types (SendEff)
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getBody)
import Node.Express.Types (EXPRESS)
import Prelude (Unit, bind, join, map, pure, ($))


sendResponse :: forall e. Response -> MessageEffs e Unit
sendResponse response = case response of
  Text (Tuple id msg)  -> log "hey"
  Image (Tuple id msg) -> log "hey"
  Audio (Tuple id msg) -> log "hey"
  Video (Tuple id msg) -> log "hey"



getEventAction :: Messaging -> Maybe EventAction
getEventAction (Messaging ms) =
  case unNullOrUndefined ms.postback of
    Just postback -> Just (EventP postback)
    Nothing -> do
      case unNullOrUndefined ms.message of
        Just message -> Just (EventM message)
        Nothing -> do
          case unNullOrUndefined ms.read of
            Just read -> Just (EventR read)
            Nothing -> Nothing


messageEventRunner :: forall e. (EventAction -> SendEff e (Maybe Response)) -> MessageEvent -> SendEff e Unit
messageEventRunner handler (MessageEvent messageEvent@{ object, entry})   = do
  let eventActions = join $ map (\(MessageEntry messageEntry@{ messaging }) -> map getEventAction messaging) entry
  liftEff $ traverse_ (\eventAction ->
      case eventAction of
        Nothing    -> liftEff $ info "No event"
        Just event ->
          handler event >>= (maybe (liftEff $ warn "no Response") sendResponse)
          ) eventActions

getMessageEvent :: forall e. HandlerM (express :: EXPRESS | e) (Either String MessageEvent)
getMessageEvent = do
  eitherBodyRaw <- getBody
  case eitherBodyRaw of
    Left multipleBodyErrors -> pure $ Left "error parsing request body"
    Right body -> do
      let eitherMessageEvent = runExcept $ readJSON body :: F MessageEvent
      case eitherMessageEvent of
        Left multipleJsonErrors -> pure $ Left "error decoding body to messageEvent"
        Right messageEvent -> pure $ Right messageEvent
