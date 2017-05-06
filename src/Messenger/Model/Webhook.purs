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
import Messenger.Types (AccessToken, UserId)
import Network.HTTP.Affjax (URL)
import Prelude (bind, pure, ($), Unit, unit)

newtype Webhook = Webhook
  { userId :: String
  , url :: URL
  , accessToken :: AccessToken
  }

instance decodeJsonWebhook :: DecodeJson Webhook where
  decodeJson json = do
    obj  <- decodeJson json
    userId <- obj .? "userId"
    url <-  obj .? "url"
    accessToken <-  obj .? "accessToken"
    pure $ Webhook { userId, url, accessToken }

instance encodeJsonWebhook :: EncodeJson Webhook where
  encodeJson (Webhook webhook)
    =   "userId" := webhook.userId
    ~>  "url" := webhook.url
    ~>  "accessToken" := webhook.accessToken
    ~> jsonEmptyObject

-- | possibly put in a utils module
findByUserId :: forall e a. (EncodeJson a, DecodeJson a) =>
            Database -> String ->  UserId -> Aff (db :: DB| e) (Maybe a)
findByUserId database userId colName = do
  col <- collection colName database
  (eitherAccessT :: Either Error a) <- attempt $ findOne [ "id" B.:= userId ] [] col
  case eitherAccessT of
    Left _        -> pure Nothing
    Right res -> pure $ Just res

-- | possibly put in a utils module
save' :: forall e a. (EncodeJson a, DecodeJson a) =>
            Database -> String -> a -> Aff (db :: DB| e) Unit
save' database colName obj = do
  col <- collection colName database
  insertOne obj defaultInsertOptions col
  pure unit

findWebhook :: forall e. Database -> UserId -> Aff (db :: DB| e) (Maybe Webhook)
findWebhook database userId = findByUserId database "webhooks" userId

saveWebhook :: forall e. Database -> UserId -> URL -> AccessToken -> Aff (db :: DB | e) Unit
saveWebhook database userId url accessToken =
  save' database "webhooks" (Webhook { userId, url, accessToken })
