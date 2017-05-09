module App.Types where
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Ref (REF, Ref)
import Data.Argonaut (jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Database.Mongo.Mongo (DB, Database)
import Messenger.Types (Ngrok)
import Network.HTTP.Affjax (AJAX)
import Node.Express.App (App)
import Node.Express.Handler (Handler)
import Node.Express.Types (EXPRESS)
import Node.Process (PROCESS)
import Prelude (Unit, bind, pure, ($))

foreign import data PASSPORT :: !

type GoogleStrategy =
  { clientID :: String
  , clientSecret :: String
  , callBack :: String
  }

newtype User = User
  { id :: String
  , name :: Maybe String
  , email :: String
  , photo :: Maybe String
  , gender :: Maybe String
  }

type Users = Array User

type UserId = String

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj  <- decodeJson json
    name <- obj .? "name"
    email <-  obj .? "email"
    id <-  obj .? "id"
    photo <- obj .? "photo"
    gender <- obj .? "gender"
    pure $ User { id, name, email, gender, photo }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user)
    =   "name" := user.name
    ~>  "email" := user.email
    ~>  "photo" := user.photo
    ~>  "gender" := user.gender
    ~>  "id" := user.id
    ~> jsonEmptyObject

type AppDb = Either Error Database

type DbRef = Ref AppDb

type AppSetupEffs e = App (console :: CONSOLE, err :: EXCEPTION, ref :: REF, ajax :: AJAX
    , ngrok :: Ngrok, passport :: PASSPORT, process :: PROCESS, db :: DB | e)

type JWTSecret = String

type AppEffs e a = Eff (err :: EXCEPTION, console :: CONSOLE, ref :: REF, ajax :: AJAX
    , ngrok :: Ngrok, express :: EXPRESS, passport :: PASSPORT, process :: PROCESS, db :: DB | e ) a

type AuthEffs e a = Eff (ref :: REF, console :: CONSOLE, err :: EXCEPTION, db :: DB | e) a

type HandlerAuthEffs e = Handler (ref :: REF, console :: CONSOLE, err :: EXCEPTION, db :: DB | e)

type WebhookEffs e = Handler (ref :: REF, console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION, db :: DB | e)

type ExitEffs e =  Eff (ajax :: AJAX, ngrok :: Ngrok, console :: CONSOLE, process :: PROCESS, err :: EXCEPTION | e) Unit

type AddWebHookEffs e = Handler (ref :: REF, console :: CONSOLE, ngrok :: Ngrok, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS, db :: DB | e)

type JWToken = { token :: String }
