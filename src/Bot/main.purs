module Bot.Main where

import Bot.MessageEvent (MessageEvent)
import Bot.Types (MessageResponse, SenderId)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, renderForeignError)
import Data.Foreign.Class (readJSON)
-- import Data.List.Types (toList)
import Network.HTTP.Affjax (AJAX)
import Node.Express.Handler (Handler)
import Node.Express.Request (getBody)
import Node.Express.Response (send)
import Prelude (Unit, bind, id, map, ($), (>>>), const)

type SendAction e = SenderId -> MessageResponse -> Eff (ajax :: AJAX, console :: CONSOLE | e) Unit

type MessageEventAction e = MessageEvent -> Handler (ajax :: AJAX, console :: CONSOLE  | e)


webhookMessagesHandler :: forall e. MessageEventAction e -> Handler (ajax :: AJAX, console :: CONSOLE  | e)
webhookMessagesHandler action = do
  eitherBodyRaw <- getBody
  case eitherBodyRaw of
    Left _ -> do
      liftEff $ error "multipleErrors"
      send("failed")
    Right body -> do
      let eitherMessageEvent = runExcept $ readJSON body :: F MessageEvent
      case eitherMessageEvent of
        Left _ -> do
          liftEff $ error "failed to purse json"
          send("failed")
        Right messageEvent -> action messageEvent
