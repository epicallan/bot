module Messenger.Model.Webhook (
    findWebhook
  , saveWebhook
  , Webhook(..)
  )where

import Database.Mongo.Bson.BsonValue as B
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.Mongo.Mongo (DB, Database, collection, findOne, insertOne)
import Database.Mongo.Options (defaultInsertOptions)
import Messenger.Types (UserId)
import Network.HTTP.Affjax (URL)
import Prelude (bind, pure, ($), Unit, unit)

newtype Webhook = Webhook
  { userId :: String
  , url :: URL
  }

instance decodeJsonWebhook :: DecodeJson Webhook where
  decodeJson json = do
    obj  <- decodeJson json
    userId <- obj .? "userId"
    url <-  obj .? "url"
    pure $ Webhook { userId, url }

instance encodeJsonWebhook :: EncodeJson Webhook where
  encodeJson (Webhook webhook)
    =   "userId" := webhook.userId
    ~>  "url" := webhook.url
    ~> jsonEmptyObject

findWebhook :: forall e. Database -> UserId -> Aff (db :: DB| e) (Maybe Webhook)
findWebhook database userId = do
  col <- collection "webhooks" database
  (eitherWbHook :: Either Error Webhook) <- attempt $ findOne [ "id" B.:= userId ] [] col
  case eitherWbHook of
    Left _        -> pure Nothing
    Right webhook -> pure $ Just webhook

saveWebhook :: forall e. Database -> UserId -> URL -> Aff (db :: DB | e) Unit
saveWebhook database userId url = do
  col <- collection "webhooks" database
  insertOne (Webhook { userId, url }) defaultInsertOptions col
  pure unit
