module App.Model.User where
import Database.Mongo.Bson.BsonValue as B
import Control.Monad (void)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.List (List)
import Database.Mongo.Mongo (DB, Database, collect, collection, find, findOne, insertOne)
import Database.Mongo.Options (defaultInsertOptions)
import Prelude (bind, pure, Unit, unit, ($))

newtype User = User
  { id :: String
  , name :: String
  , email :: String
  , photo :: String
  , gender :: String
  }

type Users = List User

userCol = "users" :: String

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

addUser :: forall e. Database -> User -> Eff (db :: DB, err :: EXCEPTION | e) Unit
addUser database user =
  void $ launchAff $ do
    col <- collection userCol database
    insertOne user defaultInsertOptions col
    pure unit

findUser :: forall e. User -> Database -> Aff (db :: DB, console :: CONSOLE | e) User
findUser (User user) database = do
  col <- collection userCol database
  userRes <- findOne [ "id" B.:= user.id ] [] col
  pure $ (userRes :: User)

findUsers :: forall e. Database -> Aff (db :: DB, console :: CONSOLE | e) Users
findUsers database = do
  liftEff $ log "getting user"
  col <- collection userCol database
  cur <- find [] [] col
  res <- collect cur
  pure $ (res :: Users)
