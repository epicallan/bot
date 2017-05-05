module Messenger.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Database.Mongo.Mongo (DB)
import Messenger.Foreign (Ngrok)
import Network.HTTP.Affjax (AJAX, URL)
import Prelude (class Show, Unit)

type FbBase = URL

type SenderId = String

type UserId = String

type AccessToken = String

type WebHookSetUpEffs e = Eff (ngrok:: Ngrok, ajax :: AJAX, db :: DB, err:: EXCEPTION, console :: CONSOLE | e) Unit
type WebHookSetUpAff e = Aff (ngrok:: Ngrok, ajax :: AJAX, db :: DB, console :: CONSOLE | e) Unit

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
