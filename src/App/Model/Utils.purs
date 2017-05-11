module App.Model.Utils where

import Database.Mongo.Bson.BsonValue as B
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.Mongo.Mongo (DB, Database, collection, findOne, insertOne)
import Database.Mongo.Options (defaultInsertOptions)
import App.Types (UserId)
import Prelude (bind, pure, ($), Unit, unit)


findByUserId :: forall e a. EncodeJson a => DecodeJson a =>
            Database -> String ->  UserId -> Aff (db :: DB| e) (Maybe a)
findByUserId database colName userId = do
  col <- collection colName database
  (eitherAccessT :: Either Error a) <- attempt $ findOne [ "id" B.:= userId ] [] col
  case eitherAccessT of
    Left _        -> pure Nothing
    Right res -> pure $ Just res

save' :: forall e a. EncodeJson a => DecodeJson a =>
            Database -> String -> a -> Aff (db :: DB| e) Unit
save' database colName obj = do
  col <- collection colName database
  _ <- insertOne obj defaultInsertOptions col
  pure unit
