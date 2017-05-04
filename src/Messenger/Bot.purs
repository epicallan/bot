module Messenger.Bot where
import Control.Bind ((>>=))
import Control.Monad.Eff (foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, info, log)
import Control.Monad.Except (runExcept)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (F, MultipleErrors)
import Data.Foreign.Class (class IsForeign, readJSON)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Messenger.Model.MessageEvent (EventAction(..), MessageEffs, MessageEntry(..), MessageEvent(..), Messaging(..), Postback(..), Response(..), WebHookEffs)
import Node.Express.Request (getBody)
import Node.Express.Response (setStatus)
import Prelude (Unit, bind, const, join, map, pure, void, ($), (<>))
import Utils (multpleErrorsToStr)


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


messageEventRunner :: forall e. (EventAction -> MessageEffs e (Maybe Response)) -> MessageEvent -> MessageEffs e Unit
messageEventRunner handler (MessageEvent messageEvent@{ object, entry}) = do
  let eventActions = join $ map (\(MessageEntry messageEntry@{ messaging }) -> map getEventAction messaging) entry
  case (head eventActions) of
    Nothing -> liftEff $ log "No event"
    Just maybeAction -> do
      case maybeAction of
        Nothing -> liftEff $ log "No event"
        Just event -> do
          liftEff $ log "handle event action"
          maybeRes <- handler event
          case maybeRes of
            Nothing ->  liftEff $ log "no Response"
            Just res -> sendResponse res
