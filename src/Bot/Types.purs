module Bot.Types where

import Bot.MessageEvent (MessageEvent)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax (AJAX)
import Node.Express.Handler (Handler)
import Prelude (class Show, Unit)

type FbBase = URL

data MessageResponse = Text String | Image String | Typing

type SenderId = String

type AccessToken = String

type SendAction e = SenderId -> MessageResponse -> Eff (ajax :: AJAX, console :: CONSOLE | e) Unit

type MessageEventAction e = MessageEvent -> Handler (ajax :: AJAX, console :: CONSOLE  | e)


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
