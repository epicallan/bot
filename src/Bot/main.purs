module Bot.Main where

import Bot.Types (MessageEvent, SenderId)
-- import Control.Monad (void)
-- import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Exception (message)
-- import Control.Monad.Except (runExcept)
-- import Data.Argonaut (Json)
-- import Data.Argonaut.Encode (encodeJson)
-- import Data.Either (either)
-- import Data.Foreign (Foreign, renderForeignError, unsafeFromForeign)
-- import Data.List (List(..), (:))
-- import Data.List.NonEmpty (toList)
import Data.StrMap (StrMap)
import Database.Mongo.Mongo (DB)
-- import Node.Express.Handler (Handler)
-- import Node.Express.Request (getBody)
import Prelude (Unit)

type SendAction e = SenderId -> String -> Eff (db :: DB, console :: CONSOLE | e) Unit

type SendActionMap e = StrMap (SendAction e)

type MessagingEventAction e = MessageEvent -> Eff (db :: DB, console :: CONSOLE | e) Unit

type MessagingEventActionMap e = StrMap (MessagingEventAction e)

-- receivedMessageAction :: MessagingEventAction e
-- receivedMessageAction = pure
--
-- receivedMessagReadAction :: forall e. MessagingEventAction e
-- receivedMessagReadAction = pure
--
-- receivedPostbackAction :: forall e. MessagingEventAction e
-- receivedPostbackAction =  pure
--
-- messagingEventActions :: forall e. MessagingEventActionMap e
-- messagingEventActions = fromFoldable $ Nil
--   : ("message" receivedMessageAction)
--   : ("postback" receivedPostbackAction)
--   : ("read" receivedMessagReadAction)

-- processMessageBody :: forall e. MessageBody -> Eff (db :: DB, console :: CONSOLE | e) Unit
-- processMessageBody body = pure
--
-- webhookMessagesHandler :: forall e. Handler (db :: DB, console :: CONSOLE | e) Unit
-- webhookMessagesHandler = do
--   eitherBody <- liftEff $ getBody
--   case eitherBody of
--     Left multipleErrors -> fmap $ lifttEff >>> log >>> renderForeignError $ toList body
--     Right body -> do
--       eitherBody <- runExcept $ (decode body :: MessageBody)
--       either (const $ (liftEff $ log $ message err)) processMessageBody eitherBody
