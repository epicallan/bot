module App.Model.User where
import Database.Mongo.Bson.BsonValue as B
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (error, log)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.Mongo.Mongo (DB, Database, Collection, collection, find, findOne, collect)
import Prelude (bind, ($), pure, Unit, (>))
import Unsafe.Coerce (unsafeCoerce)


newtype User = User
  { id :: String
  , username :: String
  , email :: String
  }


instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj  <- decodeJson json
    username <- obj .? "username"
    email <-  obj .? "email"
    id <-  obj .? "id"
    pure $ User { id, username, email }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user)
    =   "name" := user.username
    ~>  "email" := user.email
    ~>  "id" := user.id
    ~> jsonEmptyObject

toJson :: forall a. a -> Json
toJson = unsafeCoerce

-- findUser :: forall e. Database -> Aff (db :: DB | e) (Maybe User)
-- findUser db = do
--   col <- collection "user" db
--   cur <- find [ "name" B.:= "Wow" ] [ "name" B.:= 1.0 ] col
--   res <- collect cur
--   let eitherUser = decodeJson $ toJson res
--   case eitherUser of
--     (Left err) -> pure $ Nothing
--     (Right user) -> pure $ (Just user)

findUser :: forall e. Database -> Aff (db :: DB | e) User
findUser database = do
  col <- collection "events" database
  cur <- findOne [ "name" B.:= "Wow" ] [ "name" B.:= 1.0 ] col
  res <- collect cur
  pure $ (res :: User)
