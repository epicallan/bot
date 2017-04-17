module App.Handler.User where

import App.Foreign (PASSPORT, facebookAuth)
import App.Model.User (User)
import App.Types (DbRef)
import Control.Monad.Eff.Exception (message)
import Database.Mongo.Bson.BsonValue ((:=))
import Database.Mongo.Mongo (DB, Database, Collection, collection, find, collect)
import Node.Express.Handler (Handler)
import Prelude (Unit)
import Prelude (bind, ($))

authHandler :: forall e. Handler (db :: DB, passport :: PASSPORT | e)
authHandler = do
    liftEff $ log "index handler"
    eitherDb <- liftEff $ readRef dbRef
    case eitherDb of
      Left err -> send "Error connecting to the DB: " <> message err
      Right db ->  send "connected to Db"
