module App.Model.User where
import Control.Monad (void, (*>))
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, info)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Database.Mongo.Mongo (DB, Database, collect, collection, find, insertOne)
import Database.Mongo.Options (defaultInsertOptions)
import Prelude (Unit, bind, pure, unit, ($), (<<<), const, (<>))
import App.Model.Utils (findByUserId)
import App.Types (User(..), Users)
import Data.Array hiding (find)

userCol = "users" :: String

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


findUsers :: forall e. Database -> Aff (db :: DB, console :: CONSOLE | e) (Maybe Users)
findUsers database = do
  liftEff $ log "getting user"
  col <- collection userCol database
  cur <- find [] [] col
  (eitherUsers :: Either Error Users) <- attempt $ collect cur
  either (pure <<< const Nothing) (pure <<< Just) eitherUsers
