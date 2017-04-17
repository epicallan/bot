module App.Foreign where

import App.Config.Config (FacebookStrategy)
import App.Types (SessionOptions, DbRef)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Data.Foreign (Foreign)
import Data.Foreign.Null (Null)
import Data.Function.Uncurried (Fn3)
import Database.Mongo.Mongo (DB)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (ExpressM, Request, Response)
import Prelude (Unit, ($))
foreign import data PASSPORT :: !

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import morgan :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import cookieParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import expressSession :: forall e. SessionOptions -> Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import passportInitialize :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import facebookAuthStrategy :: forall e. FacebookStrategy ->  Eff (passport:: PASSPORT | e) Unit

foreign import _facebookAuth :: forall e. (Null String -> Foreign -> Eff e Unit)
                              -> Request -> Response -> (ExpressM e Unit) -> (ExpressM e Unit)

facebookAuth :: forall e. DbRef
            -> (DbRef -> Null String -> Foreign -> Eff (db :: DB, ref :: REF | e) Unit)
            -> Handler (db :: DB, ref :: REF | e)
facebookAuth dbRef authenticate = HandlerM \req resp next ->
    liftEff $ _facebookAuth (\err user -> authenticate dbRef err user) req resp next
