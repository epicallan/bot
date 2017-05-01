module Bot.Action.MessageAction where

import Bot.Model.MessageEvent (MessageEffs, MessageEntry(..), MessageEvent(..), MessageEventHandler, Messaging(..))
import Data.Foldable (traverse_)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..))
import Node.Express.Response (setStatus)
-- import Prelude (($))

processMessaging :: forall e. MessageEntry -> Messaging -> MessageEventHandler e -> MessageEffs e
processMessaging mE (Messaging ms) handler = do
  case unNullOrUndefined ms.postback of
    Just postback -> handler.postback mE postback
    Nothing -> do
      case unNullOrUndefined ms.message of
        Just message -> handler.message mE message
        Nothing -> setStatus 200

-- processMessaging :: forall e. MessageEntry -> Messaging -> MessageEventHandler e -> MessageEffs e
-- processMessaging mE (Messaging ms) handler = do
--   unNullOrUndefined ms.postback >>= handler.postback mE <|>
--   unNullOrUndefined ms.message  >>=  handler.message mE <|>
--   setStatus 200

messageActionRunner :: forall e. MessageEventHandler e -> MessageEvent -> MessageEffs e
messageActionRunner handler (MessageEvent messageEvent@{ object, entry}) =
  traverse_ (\(MessageEntry messageEntry@{ messaging })
    -> traverseMessaging (MessageEntry messageEntry) messaging) entry
    where traverseMessaging mE = traverse_ (\m -> processMessaging mE m handler)
