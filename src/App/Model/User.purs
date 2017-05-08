module App.Model.User where
import Database.Mongo.Bson.BsonValue as B
import Control.Monad (void, (*>))
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, info)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Argonaut (jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Database.Mongo.Mongo (DB, Database, collect, collection, find, findOne, insertOne)
import Database.Mongo.Options (defaultInsertOptions)
import Prelude (Unit, bind, pure, unit, ($), (<<<), const, (<>))
import Utils (findByUserId)
import Data.Array hiding (find)

newtype User = User
  { id :: String
  , name :: Maybe String
  , email :: String
  , photo :: Maybe String
  , gender :: Maybe String
  }

type Users = Array User
type UserId = String

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

-- | creates user if user doesnt exit
createUser :: forall e. Database -> User -> Eff (db :: DB, console :: CONSOLE, err :: EXCEPTION | e) Unit
createUser database user@(User u) =
  void $ launchAff $ do
    col <- collection userCol database
    (maybeUser :: Maybe User) <- (findByUserId database u.id userCol)
    case maybeUser of
      Nothing -> (insertOne user defaultInsertOptions col) *> pure unit
      Just (User dbUser) ->
        (liftEff $ info $ "user exist" <> dbUser.id ) *> pure unit

findUser :: forall e. UserId -> Database -> Aff (db :: DB, console :: CONSOLE | e) (Maybe User)
findUser userId database = do
  col <- collection userCol database
  (eitherUser :: Either Error User) <- attempt $ findOne [ "id" B.:= userId ] [] col
  either (pure <<< const Nothing) (pure <<< Just) eitherUser

findUsers :: forall e. Database -> Aff (db :: DB, console :: CONSOLE | e) (Maybe Users)
findUsers database = do
  liftEff $ log "getting user"
  col <- collection userCol database
  cur <- find [] [] col
  (eitherUsers :: Either Error Users) <- attempt $ collect cur
  either (pure <<< const Nothing) (pure <<< Just) eitherUsers
