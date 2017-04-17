module App.Handler.User where
import App.Model.User (User(..), addUser)
import App.Types (DbRef)
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
import Prelude (Unit, ($), pure, bind, (<>))

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



-- TODO change type signature so that we can throw error to be caught by error handdler
authHandler :: forall e. DbRef -> Null String -> Foreign
            -> Eff (ref :: REF, console :: CONSOLE, err :: EXCEPTION, db :: DB | e) Unit
authHandler dbRef nullableError userRes = do
      let maybeError = unNull nullableError
      case maybeError of
        Just err -> liftEff $ log $ "Unexpected authentication Error"
        Nothing -> do
          eitherDb <- liftEff $ readRef dbRef
          case eitherDb of
            Left err -> log $ "Error connecting to the database for authentication " <> message err
            Right db -> maybe (log $ "UnExpected user parse error") (addUser db) $ parseUserRes userRes
