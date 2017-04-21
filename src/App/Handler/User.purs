module App.Handler.User where
import App.Model.User (User(..), addUser)
import App.Types (DbRef, JWToken, AuthEffs)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Eff.Ref (REF, readRef)
import Data.Argonaut (Json, (.?))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Foreign.Null (Null, unNull)
import Data.Maybe (Maybe(..), maybe)
import Database.Mongo.Mongo (DB)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)
import Prelude (Unit, ($), pure, bind, (<>))

-- TODO  feels like i have a lot of boiler plate in here with all the json decoding should look into
-- Getting rid most of it

newtype Email = Email
  { email :: String
  , emailType :: String
  }

newtype UserRaw = UserRaw
  { id :: String
  , name :: String
  , emails :: Array Email
  }

instance decodeJsonEmail :: DecodeJson Email where
  decodeJson json = do
    obj  <- decodeJson json
    email <- obj .? "email"
    emailType <-  obj .? "type"
    pure $ Email { email, emailType }

instance decodeJsonUserRaw :: DecodeJson UserRaw where
  decodeJson json = do
    obj  <- decodeJson json
    name <- obj .? "displayName"
    emails <-  obj .? "emails"
    id <-  obj .? "id"
    pure $ UserRaw { id, name, emails }

unsafelyToJson :: Foreign -> Json
unsafelyToJson = unsafeFromForeign

parseUserRes :: Foreign -> Maybe User
parseUserRes userRes =
    let eitherUserRaw = decodeJson $ unsafelyToJson userRes
    in case eitherUserRaw of
        Left  _                 -> Nothing
        Right (UserRaw userRaw) -> maybe Nothing (addEmail userRaw) $ head userRaw.emails
        where addEmail u (Email e) = Just (User {id : u.id, name : u.name, email : e.email})

-- TODO change type signature so that we can throw error to be caught by a error handdler
-- middleware
jwtoken = { token : "null"} :: JWToken

authHandler :: forall e. DbRef -> Foreign -> AuthEffs e JWToken
authHandler dbRef userRes = do
    eitherDb <- liftEff $ readRef dbRef
    case eitherDb of
      Left err -> do
        liftEff $ log $ "Error connecting to the database for authentication " <> message err
        jwtoken
      Right _ -> jwtoken



loginHandler :: forall e. Handler e
loginHandler = send "Please go and login" -- TODO redirect to login page on front end app



indexHandler :: forall e. Handler e
indexHandler = send "You have been signed up" -- TODO redirect to login page on front end app
