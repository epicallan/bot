module Bot.Types where

import Control.Monad.Eff (Eff)
import Data.Argonaut (jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Prelude (class Show, Unit)
type FbWebHook = String

type FbBase = String

type FbPath = FbBase -> String -> String

newtype MessageEvent = MessageEvent { type :: Maybe String }

newtype FbWebHookRequest = FbWebHookRequest
  { object :: String
  , callbackUrl :: String
  , verifyToken :: String
  , fields :: Array String
  }

data Message = Text String | Image String | Typing

type SenderId = String

type SendAction e = SenderId -> Maybe Message -> Eff e Unit

type AccessToken = String

type Url = String
newtype FbGenericResponse = FbGenericResponse { success :: String }
derive instance genericFbGenericResponse :: Generic FbGenericResponse _
instance showFbGenericResponse :: Show FbGenericResponse where show = genericShow

instance isForeignFbGenericResponse :: IsForeign FbGenericResponse where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

newtype AccessTokenJson = AccessTokenJson { access_token :: String }
derive instance genericAccessTokenJson :: Generic AccessTokenJson _
instance showAccessTokenJson :: Show AccessTokenJson where show = genericShow

instance isForeignAccessTokenJson :: IsForeign AccessTokenJson where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x


instance encodeJsonFbWebHookRequest :: EncodeJson FbWebHookRequest where
  encodeJson (FbWebHookRequest fbwr)
    =   "object" := fbwr.object
    ~>  "callback_url" := fbwr.callbackUrl
    ~>  "verify_token" := fbwr.verifyToken
    ~>  "fields" := fbwr.fields
    ~> jsonEmptyObject
