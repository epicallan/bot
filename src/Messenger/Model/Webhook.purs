module Messenger.Model.Webhook (
    findWebhook
  , saveWebhook
  )where
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Database.Mongo.Mongo (DB, Database)
import Messenger.Types (AccessToken, UserId, Webhook(..))
import Network.HTTP.Affjax (URL)
import Prelude (Unit)
import Utils (save', findByUserId)

findWebhook :: forall e. Database -> UserId -> Aff (db :: DB| e) (Maybe Webhook)
findWebhook database userId = findByUserId database "webhooks" userId

saveWebhook :: forall e. Database -> UserId -> URL -> AccessToken -> Aff (db :: DB | e) Unit
saveWebhook database id url accessToken =
  save' database "webhooks" (Webhook { id, url, accessToken })
