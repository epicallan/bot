module Messenger.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Database.Mongo.Mongo (DB)
import Messenger.Foreign (Ngrok)
import Network.HTTP.Affjax (AJAX, URL)
import Prelude (class Show, Unit, bind, ($), pure)

type FbBase = URL

type SenderId = String

type UserId = String

type AccessToken = String

type FbMessengerConf =
  { appSecret :: String
  , appId :: String
  , verifyToken :: String
  , accessToken :: AccessToken
  }

type WebHookSetUpEffs e = Eff (ngrok:: Ngrok, ajax :: AJAX, db :: DB, err:: EXCEPTION, console :: CONSOLE | e) Unit
type WebHookSetUpAff e = Aff (ngrok:: Ngrok, ajax :: AJAX, db :: DB, console :: CONSOLE | e) Unit

type SendEff e a = Eff (ajax :: AJAX, console :: CONSOLE, err :: EXCEPTION | e) a

newtype Webhook = Webhook
  { id :: String
  , url :: URL
  }

instance decodeJsonWebhook :: DecodeJson Webhook where
  decodeJson json = do
    obj  <- decodeJson json
    id <- obj .? "id"
    url <-  obj .? "url"
    pure $ Webhook { id, url}

instance encodeJsonWebhook :: EncodeJson Webhook where
  encodeJson (Webhook webhook)
    =   "id" := webhook.id
    ~>  "url" := webhook.url
    ~> jsonEmptyObject


-- | post response from send API
newtype SendResponse = SendResponse
  { recipient_id :: String
  , message_id :: String
  }

derive instance genericSendResponse :: Generic SendResponse _
instance showSendResponse :: Show SendResponse where show = genericShow
instance isForeignSendResponse :: IsForeign SendResponse where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

newtype FbGenericResponse = FbGenericResponse { success :: Boolean }

derive instance genericFbGenericResponse :: Generic FbGenericResponse _
instance showFbGenericResponse :: Show FbGenericResponse where show = genericShow
instance isForeignFbGenericResponse :: IsForeign FbGenericResponse where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

newtype AccessTokenJson = AccessTokenJson
  { access_token :: String
  , token_type   :: String
  }

derive instance genericAccessTokenJson :: Generic AccessTokenJson _
instance showAccessTokenJson :: Show AccessTokenJson where show = genericShow
instance isForeignAccessTokenJson :: IsForeign AccessTokenJson where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

newtype FbWebhookRequest = FbWebhookRequest
  { object :: String
  , callback_url :: String
  , verify_token :: String
  , fields :: Array String
  }

instance encodeJsonFbWebhookRequest :: EncodeJson FbWebhookRequest where
  encodeJson (FbWebhookRequest fbwr)
    =   "object" := fbwr.object
    ~>  "callback_url" := fbwr.callback_url
    ~>  "verify_token" := fbwr.verify_token
    ~>  "fields" := fbwr.fields
    ~> jsonEmptyObject
