module Bot.Action.MessageAction where

import Bot.Model.MessageEvent (EventAction(..), MessageEffs, MessageEntry(..), MessageEvent(..), MessageEventHandler, Messaging(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (info)
import Data.Foldable (traverse_)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..))
import Node.Express.Response (setStatus)
import Prelude (($), bind)

processMessaging :: forall e. MessageEntry -> Messaging -> MessageEventHandler e -> MessageEffs e
processMessaging mE (Messaging ms) handler = do
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

messageActionRunner :: forall e. MessageEventHandler e -> MessageEvent -> MessageEffs e
messageActionRunner handler (MessageEvent messageEvent@{ object, entry}) =
  traverse_ (\(MessageEntry messageEntry@{ messaging })
    -> traverseMessaging (MessageEntry messageEntry) messaging) entry
    where traverseMessaging mE = traverse_ (\m -> processMessaging mE m handler)
