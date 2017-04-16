module App.Handler.User where

-- import App.Foreign (PASSPORT, facebookAuth)
-- import App.Model.User (User)
-- import Database.Mongo.Bson.BsonValue ((:=))
-- import Database.Mongo.Mongo (DB, Database, Collection, collection, find, collect)
-- import Node.Express.Handler (Handler)
-- import Prelude (Unit)
-- import Prelude (bind, ($))
--
-- -- authCallback :: User -> Eff (passport:: PASSPORT, db :: DB | e) Unit
-- -- authCallback =
--
-- authHandler :: forall e. Database -> Handler (db :: DB | e)
-- authHandler db = do
--   col <- collection "user" db
--   cur <- find [ "name" := "Wow" ] [ "name" := 1.0 ] col
--   res <- collect cur
--   liftEff $ assert $ length (res :: Array User) > 0
