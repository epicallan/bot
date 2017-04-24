module Bot.Main where

import App.Model.User (UserId)
import Bot.Config (FbMessengerConf)
import Control.Monad.Eff (Eff)
import Database.Mongo.Mongo (DB)
import Node.Express.Handler (Handler)
import Prelude (pure)

type FbWebHook  = String

fbBase = "https://graph.facebook.com/v2.7/oauth/access_token" :: String

data ReceiveAction e a = ReceiveAction (Foreign -> Eff e a) 

type MessageActions =
  { message :: ReceiveAction
  , postback :: ReceiveAction
  }

messageAction :: forall e a. Foreign -> Eff e a

initMessageActions :: MessageActions
initMessageActions =
  { message  : messageAction
  , postback : postbackAction
  }

setUpNewWebHook :: forall e. FbWebHook -> FbMessengerConf -> Handler e
setUpNewWebhook = pure

createFbWebHooK :: forall e. UserId -> Eff (db :: DB, ngrok:: Ngrok | e) FbWebHook
createFbWebHook = pure ""
