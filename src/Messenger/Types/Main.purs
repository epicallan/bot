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

type WebHookSetUpEffs e = Eff (ngrok:: Ngrok, ajax :: AJAX, db :: DB, err:: EXCEPTION, console :: CONSOLE | e) Unit
type WebHookSetUpAff e = Aff (ngrok:: Ngrok, ajax :: AJAX, db :: DB, console :: CONSOLE | e) Unit

type SendEff e a = Eff (ajax :: AJAX, console :: CONSOLE, err :: EXCEPTION | e) a

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

newtype AccessTokenJson = AccessTokenJson { token :: String }

derive instance genericAccessTokenJson :: Generic AccessTokenJson _
instance showAccessTokenJson :: Show AccessTokenJson where show = genericShow
instance isForeignAccessTokenJson :: IsForeign AccessTokenJson where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

newtype FbWebhookRequest = FbWebhookRequest
  { object :: String
  , callbackUrl :: String
  , verifyToken :: String
  , fields :: Array String
  }

instance encodeJsonFbWebhookRequest :: EncodeJson FbWebhookRequest where
  encodeJson (FbWebhookRequest fbwr)
    =   "object" := fbwr.object
    ~>  "callback_url" := fbwr.callbackUrl
    ~>  "verify_token" := fbwr.verifyToken
    ~>  "fields" := fbwr.fields
    ~> jsonEmptyObject
