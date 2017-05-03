module Messenger.Types where

import Data.Argonaut (jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Network.HTTP.Affjax (URL)
import Prelude (class Show)

type FbBase = URL

type SenderId = String

type UserId = String

type AccessToken = String


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
