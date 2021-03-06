module Messenger.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Network.HTTP.Affjax (AJAX, URL)
import Prelude (class Show, Unit)

foreign import data Ngrok :: Effect

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

type WebHookSetUpEffs e = Eff (ngrok:: Ngrok, ajax :: AJAX, exception :: EXCEPTION, console :: CONSOLE | e) Unit

type WebHookSetUpAff e = Aff (ngrok:: Ngrok, ajax :: AJAX, console :: CONSOLE | e) Unit

type SubcribeAff e = Aff (ajax :: AJAX, console :: CONSOLE | e) Unit

type SendEff e a = Eff (ajax :: AJAX, console :: CONSOLE, exception :: EXCEPTION | e) a

-- | post response from send API
newtype SendResponse = SendResponse
  { recipient_id :: String
  , message_id :: String
  }

derive instance genericSendResponse :: Generic SendResponse _
instance showSendResponse :: Show SendResponse where show = genericShow
instance decodeSendResponse :: Decode SendResponse where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

newtype FbGenericResponse = FbGenericResponse { success :: Boolean }

derive instance genericFbGenericResponse :: Generic FbGenericResponse _
instance showFbGenericResponse :: Show FbGenericResponse where show = genericShow
instance decodeFbGenericResponse :: Decode FbGenericResponse where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

newtype AccessTokenJson = AccessTokenJson
  { access_token :: String
  , token_type   :: String
  }

derive instance genericAccessTokenJson :: Generic AccessTokenJson _
instance showAccessTokenJson :: Show AccessTokenJson where show = genericShow
instance decodeAccessTokenJson :: Decode AccessTokenJson where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

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
