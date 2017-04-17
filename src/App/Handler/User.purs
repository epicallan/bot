module App.Handler.User where
-- import App.Model.User (User, addUser)
import App.Types (DbRef)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (message)
import Data.Foreign (Foreign)
import Data.Foreign.Null (Null)
-- import Data.Maybe (maybe)
-- import Database.Mongo.Bson.BsonValue ((:=))
import Database.Mongo.Mongo (DB)
import Prelude (Unit, unit, ($), pure)

authHandler :: forall e. DbRef -> Null String -> Foreign -> Eff (db :: DB | e) Unit
authHandler dbRef err userRes = pure $ unit




-- authHandler :: forall e. DBRef -> (Null String -> Foreign -> Eff (db :: DB | e) Unit)
-- authHandler dbRef = \ maybeError userRes -> do
--     maybeError <- unNull err
--     case maybeError of
--       Just err -> send "Unexpected authentication Error"
--       Nothing -> do
--         eitherDb <- liftEff $ readRef dbRef
--         case eitherDb of
--           Left err -> send "Error connecting to the database for authentication " <> message err
--           Right db ->  do
--             maybeUser <- parseRes authRes
--             maybe (send "UnExpected user parse error") (addUser db)
--             redirect "/auth/facebook/return"
