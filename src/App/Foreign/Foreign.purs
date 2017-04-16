module App.Foreign where

-- import App.Config.Config (FacebookStrategy)
-- import App.Model.User (User)
-- import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3)
import Database.Mongo.Mongo (Database)
import Node.Express.Types (ExpressM, Request, Response)
import Prelude (Unit)

foreign import data PASSPORT :: !


foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import morgan :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import cookieParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import expressSession :: forall e. Database -> Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import passportSession :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import passportInitialize :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

-- foreign import facebookAuth :: forall e. (User -> Eff (passport:: PASSPORT, db :: DB | e) Unit )
--                                               ->  FacebookStrategy
--                                               ->  Eff (passport:: PASSPORT, db :: DB | e) Unit
