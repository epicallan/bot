module App.Model.User where
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Database.Mongo.Mongo (find)
import Prelude (Unit, unit)

type User =
  { id :: String
  , displayName :: String
  , email :: String
  , session :: String
  }

init :: User
init =
  { id : ""
  , displayName : ""
  , email : ""
  , session : ""
  }

createUser :: forall e. UserCollection -> User -> Eff(db :: DB, console :: CONSOLE | e) Unit
createUser col user = do
  cur <- find ["id" := user.id] col
  res <- collect cur
  liftEff $ log $ res
  pure unit
